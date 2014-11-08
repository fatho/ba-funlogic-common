{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
module Language.SaLT.TypeChecker where

import           Control.Applicative
import           Control.Lens
import           Control.Monad             hiding (mapM, mapM_)
import           Control.Monad.Reader      hiding (mapM, mapM_)
import           Data.Foldable
import qualified Data.Map                  as M
import           Data.Traversable
import           Prelude                   hiding (any, foldr, mapM, mapM_)

import           FunLogic.Core.TypeChecker
import           Language.SaLT.AST

-- * Built-Int types

pattern TSet x = TCon "Set" [x]

builtInTyCons :: M.Map Name Kind
builtInTyCons = M.fromList
  [ ("Set", KFun KStar KStar)
  , ("Nat", KStar)
  , ("->", KFun KStar (KFun KStar KStar))
  ]

builtInADTs :: M.Map Name ADT
builtInADTs = M.fromList $ map (\adt -> (adt^.adtName, adt)) [adtDefBool, adtDefList, adtDefPair]

-- * Type Checking

includeBuiltIns :: TC ()
includeBuiltIns = do
  typeScope %= M.union builtInTyCons
  typeScope %= M.union (adtKind <$> builtInADTs)
  topScope  %= M.union (M.unions $ map adtConstructorTypes $ M.elems builtInADTs)

-- | Typechecks a module.
checkModule :: Module -> TC ()
checkModule saltMod = do
  -- check kinds of ADT definitions
  typeScope %= M.union (adtKind <$> saltMod^.modADTs)
  mapM_ checkADT (saltMod^.modADTs)
  -- check all top level bindings
  topScope %= M.union (view bindingType <$> saltMod^.modBinds)
  topScope %= M.union (M.unions $ map adtConstructorTypes $ M.elems $ saltMod^.modADTs)
  mapM_ checkBinding (saltMod^.modBinds)

-- | Typechecks a single top level binding
checkBinding :: Binding -> TC ()
checkBinding bnd = local (errContext.errSrc .~ Just (bnd^.bindingSrc)) $ do
  bodyTy <- tcExp $ bnd^.bindingExpr
  let (TyDecl _ _ ty) = bnd^.bindingType
  void $ checkKind ty
  assertTypesEq bodyTy ty

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
  Just decl -> do
    mapM_ checkKind tyArgs
    instantiate tyArgs decl -- TODO: check context

tcExp (ELam argName argTy body) = do
  void $ checkKind argTy
  bodyTy <- local (localScope.at argName .~ Just argTy) (tcExp body)
  return (TFun argTy bodyTy)

tcExp (EApp callee arg) = do
  argTy    <- tcExp arg
  tcExp callee >>= \case
    TFun funArg funDest -> do
      assertTypesEq funArg argTy
      return funDest
    calleeTy -> errorTC $ ErrTypeMismatch calleeTy (TFun argTy (TVar "<result>"))

tcExp (ELit (LInt _)) = return TNat

tcExp (EPrim PrimAdd [x, y]) = do
  tcExp x >>= assertTypesEq TNat
  tcExp y >>= assertTypesEq TNat
  return TNat

tcExp (EPrim PrimEq [x, y]) = do
  tcExp x >>= assertTypesEq TNat
  tcExp y >>= assertTypesEq TNat
  return (TCon "Bool" [])

tcExp (EPrim PrimBind [x, y]) = do
  TSet elTy                 <- tcExp x
  TFun argTy (TSet resElTy) <- tcExp y
  assertTypesEq elTy argTy
  return (TSet resElTy)

tcExp e@(EPrim _ _) = errorTC $ ErrGeneral $ "Wrong use of primitive operation: " ++ show e

tcExp (ECon con tyArgs) = use (topScope.at con) >>= \case
  Nothing -> errorTC (ErrConNotInScope con)
  Just decl ->  do
    mapM_ checkKind tyArgs
    instantiate tyArgs decl

tcExp (ESet e) = TSet <$> tcExp e

tcExp (ECase expr alts) = do
  expTy  <- tcExp expr
  (aty:atys) <- mapM (tcAlt expTy) alts
  case find (/=aty) atys of
    Nothing -> return aty
    Just wrongTy -> errorTC $ ErrTypeMismatch aty wrongTy

tcExp (EFailed ty) = checkKind ty >> return ty

tcExp (EUnknown ty) = checkKind ty >> return ty

tcAlt :: Type -> Alt -> TC Type
tcAlt pty (Alt pat body) = case pat of
  PVar v -> local (localScope.at v .~ Just pty) $ tcExp body
  PCon c vs -> case pty of
    TVar _        -> errorTC $ ErrGeneral "cannot pattern match on unknown type"
    TCon _ tyArgs -> use (topScope.at c) >>= \case
      Nothing   -> errorTC $ ErrConNotInScope c
      Just decl -> do
        conTy <- instantiate tyArgs decl
        let (argTys, retTy) = dissectFunTy conTy
        when (length argTys /= length vs) (errorTC $ ErrGeneral "wrong number of arguments in pattern")
        when (retTy /= pty) (errorTC $ ErrTypeMismatch retTy pty)
        local (localScope %~ M.union (M.fromList $ zip vs argTys)) $ tcExp body
