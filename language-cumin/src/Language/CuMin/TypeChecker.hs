{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE PatternSynonyms            #-}
module Language.CuMin.TypeChecker where

import           Control.Applicative
import           Control.Lens
import           Control.Monad        hiding (mapM, mapM_)
import           Control.Monad.Error  hiding (mapM, mapM_)
import           Control.Monad.Reader hiding (mapM, mapM_)
import           Control.Monad.RWS    hiding (mapM, mapM_)
import           Control.Monad.State  hiding (mapM, mapM_)
import           Data.Foldable
import qualified Data.HashSet         as HS
import qualified Data.Map             as M
import           Data.Traversable
import           Prelude              hiding (foldr, mapM, mapM_, any)

import           Language.CuMin.AST
import           FunLogic.Core.TypeChecker

-- * Built-Int types

pattern TSet x = TCon "Set" [x]

builtInTyCons :: M.Map Name Kind
builtInTyCons = M.fromList
  [ ("Nat", KStar)
  , ("->", KFun KStar (KFun KStar KStar))
  ]

builtInADTs :: M.Map Name ADT
builtInADTs = M.fromList $ map (\adt -> (adt^.adtName, adt)) [adtDefBool, adtDefList, adtDefPair]

-- * Type Checking

includeBuiltIns :: TC ()
includeBuiltIns = do
  typeScope %= M.union builtInTyCons
  typeScope %= M.union (adtKind <$> builtInADTs)
  topScope  %= M.union (allConstructors (M.elems builtInADTs))

-- | Typechecks a module.
checkModule :: Module -> TC ()
checkModule saltMod = do
  -- check kinds of ADT definitions
  typeScope %= M.union (adtKind <$> saltMod^.modADTs)
  mapM_ checkADT (saltMod^.modADTs)
  -- check all top level bindings
  topScope %= M.union (view bindingType <$> saltMod^.modBinds)
  topScope %= M.union (allConstructors $ M.elems $ saltMod^.modADTs)
  mapM_ checkBinding (saltMod^.modBinds)

-- | Typechecks a single top level binding
checkBinding :: Binding -> TC ()
checkBinding bnd = do
  let (TyDecl _ _ ty) = bnd^.bindingType
  (argTys, bodyTy) <- extractArgs (bnd^.bindingArgs) ty
  realBodyTy <- local (localScope %~ M.union (M.fromList argTys)) $ tcExp $ bnd^.bindingExpr
  assertTypesEq realBodyTy bodyTy

extractArgs :: [Name] -> Type -> TC ([(Name,Type)], Type)
extractArgs [] ty             = return ([], ty)
extractArgs (x:xs) (TFun a b) = over _1 ((x,a):) <$> extractArgs xs b
extractArgs (_:_) _           = errorTC $ ErrGeneral "too many arguments for function"

tcExp :: Exp -> TC Type
tcExp (EVar vname) = view (localScope.at vname) >>= \case
  Nothing -> use (topScope.at vname) >>= \case
    Nothing -> errorTC (ErrVarNotInScope vname)
    Just (TyDecl args _ ty)
      | null args -> return ty
      | otherwise -> errorTC (ErrVarNotInScope vname)
  Just ty -> return ty

tcExp (EFun fn tyArgs) = use (topScope.at fn) >>= \case
  Nothing -> errorTC (ErrFunNotInScope fn)
  Just decl -> instanciate tyArgs decl -- TODO: check context

tcExp (EApp callee arg) = do
  argTy    <- tcExp arg
  tcExp callee >>= \case
    TFun funArg funDest -> do
      assertTypesEq funArg argTy
      return funDest
    calleeTy -> errorTC $ ErrTypeMismatch calleeTy (TFun argTy (TVar "<result>"))

tcExp (ELet var e body) = do
  varTy <- tcExp e
  local (localScope.at var .~ Just varTy) $ tcExp body

tcExp (ELetFree var ty body) =
  local (localScope.at var .~ Just ty) $ tcExp body

tcExp (ELit (LInt _)) = return TNat

tcExp (EPrim PrimAdd [x, y]) = do
  tcExp x >>= assertTypesEq TNat
  tcExp y >>= assertTypesEq TNat
  return TNat

tcExp (EPrim PrimEq [x, y]) = do
  tcExp x >>= assertTypesEq TNat
  tcExp y >>= assertTypesEq TNat
  return (TCon "Bool" [])

tcExp e@(EPrim _ _) = errorTC $ ErrGeneral $ "Wrong use of primitive operation: " ++ show e

tcExp (ECon con tyArgs) = use (topScope.at con) >>= \case
  Nothing -> errorTC (ErrConNotInScope con)
  Just decl -> instanciate tyArgs decl

tcExp (ECase expr alts) = do
  expTy  <- tcExp expr
  (aty:atys) <- mapM (tcAlt expTy) alts
  case find (/=aty) atys of
    Nothing -> return aty
    Just wrongTy -> errorTC $ ErrTypeMismatch aty wrongTy

tcExp (EFailed ty) = return ty

tcAlt :: Type -> Alt -> TC Type
tcAlt pty (Alt pat body) = case pat of
  PVar v -> local (localScope.at v .~ Just pty) $ tcExp body
  PCon c vs -> case pty of
    TVar _        -> errorTC $ ErrGeneral "cannot pattern match on unknown type"
    TCon _ tyArgs -> use (topScope.at c) >>= \case
      Nothing   -> errorTC $ ErrConNotInScope c
      Just decl -> do
        conTy <- instanciate tyArgs decl
        let (argTys, retTy) = dissectFunTy conTy
        when (length argTys /= length vs) (errorTC $ ErrGeneral "wrong number of arguments in pattern")
        when (retTy /= pty) (errorTC $ ErrTypeMismatch retTy pty)
        local (localScope %~ M.union (M.fromList $ zip vs argTys)) $ tcExp body
