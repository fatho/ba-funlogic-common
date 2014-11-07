{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE PatternSynonyms            #-}
module Language.SaLT.TypeChecker where

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

import           Language.SaLT.AST
import           FunLogic.Core.TypeChecker

-- * Built-Int types

pattern TSet x = TCon "Set" [x]

builtInTyCons :: M.Map Name Kind
builtInTyCons = M.fromList
  [ ("Set", KFun KStar KStar)
  , ("Nat", KStar)
  , ("->", KFun KStar (KFun KStar KStar))
  ]

builtInADTs :: M.Map Name ADT
builtInADTs = M.fromList $ map (\adt -> (adt^.adtName, adt))
  [ ADT "Bool" []
    [ ConDecl "False" []
    , ConDecl "True" []
    ] srcBuiltIn
  , ADT "List" ["a"]
    [ ConDecl "Nil" []
    , ConDecl "Cons" [TVar "a", TCon "List" [TVar "a"]]
    ] srcBuiltIn
  , ADT "Pair" ["a", "b"]
    [ ConDecl "Pair" [TVar "a", TVar "b"]
    ] srcBuiltIn
  ]

-- * Type Checking

allConstructors :: [ADT] -> M.Map Name TyDecl
allConstructors adts = M.unions $ map adtConstructorTypes adts

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
  bodyTy <- tcExp $ bnd^.bindingExpr
  let (TyDecl _ _ ty) = bnd^.bindingType
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
  Just decl -> instanciate tyArgs decl -- TODO: check context

tcExp (ELam argName argTy body) = do
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
  Just decl -> instanciate tyArgs decl

tcExp (ESet e) = TSet <$> tcExp e

tcExp (ECase expr alts) = do
  expTy  <- tcExp expr
  (aty:atys) <- mapM (tcAlt expTy) alts
  case find (/=aty) atys of
    Nothing -> return aty
    Just wrongTy -> errorTC $ ErrTypeMismatch aty wrongTy

tcExp (EFailed ty) = return ty

tcExp (EUnknown ty) = return ty

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

dissectFunTy :: Type -> ([Type], Type)
dissectFunTy (TFun x y) = dissectFunTy y & _1 %~ (x:)
dissectFunTy x          = ([], x)