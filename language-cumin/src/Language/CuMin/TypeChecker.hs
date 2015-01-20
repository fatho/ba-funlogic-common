{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
module Language.CuMin.TypeChecker
  ( checkModule
  , checkBinding
  , checkExp
  , checkAlt
  , CuMinErrCtx (..)
  , errExp
  , builtInTyCons
  , includeBuiltIns
  , module FunLogic.Core.TypeChecker
  ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad                hiding (mapM, mapM_)
import           Control.Monad.Reader         hiding (mapM, mapM_)
import           Control.Monad.RWS            hiding (mapM, mapM_)
import           Data.Default.Class
import           Data.Foldable
import           Data.List                    (elemIndices)
import qualified Data.Map                     as M
import qualified Data.Set                     as Set
import           Data.Traversable
import           Prelude                      hiding (any, foldr, mapM, mapM_)
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>))

import           FunLogic.Core.TypeChecker
import           Language.CuMin.AST
import           Language.CuMin.Pretty

-- * Built-In types

builtInTyCons :: M.Map TyConName Kind
builtInTyCons = M.fromList
  [ ("Nat", Kind 0)
  , ("->", Kind 2)
  ]

-- * Error Types

data CuMinErrCtx
  = CuMinErrCtx
  { _errExp :: [Exp]
  }

instance Default CuMinErrCtx where
  def = CuMinErrCtx []

instance Pretty CuMinErrCtx where
  pretty (CuMinErrCtx ectx)
    | null ectx = mempty
    | otherwise = foldMap (\e -> highlight (text "in") <+> align (prettyExp e) <> line) ectx

makeLenses ''CuMinErrCtx

-- * Type Checking

-- | Include built-in types which cannot be defined directly in CuMin.
includeBuiltIns :: TC CuMinErrCtx ()
includeBuiltIns = do
  typeScope %= M.union builtInTyCons
  dataScope %= M.insert "Nat" Set.empty

-- | Typechecks a module.
checkModule :: Module -> TC CuMinErrCtx ()
checkModule cuminMod = do
  includeBuiltIns
  -- check kinds of ADT definitions
  typeScope %= M.union (adtKind <$> cuminMod^.modADTs)
  mapM_ checkADT (cuminMod^.modADTs)
  -- derive Data instances
  dataScope %= M.union (deriveDataInstances (cuminMod^.modADTs))
  -- check all top level bindings
  topScope %= M.union (view bindingType <$> cuminMod^.modBinds)
  topScope %= M.union (M.unions $ map adtConstructorTypes $ M.elems $ cuminMod^.modADTs)
  mapM_ checkBinding (cuminMod^.modBinds)

-- | Typechecks a single top level binding
checkBinding :: Binding -> TC CuMinErrCtx ()
checkBinding bnd = let (TyDecl tvars tconstraints ty) = bnd^.bindingType in
  local ( (errContext.errSrc .~ Just (bnd^.bindingSrc))
        . (localConstraints .~ tconstraints)) $
  withTyVars tvars $ do
    checkType ty
    (argTys, bodyTy) <- extractArgs (bnd^.bindingArgs) ty
    realBodyTy <- local (localScope %~ M.union (M.fromList argTys)) $ checkExp $ bnd^.bindingExpr
    assertTypesEq realBodyTy bodyTy

extractArgs :: [VarName] -> Type -> TC CuMinErrCtx ([(VarName,Type)], Type)
extractArgs [] ty             = return ([], ty)
extractArgs (x:xs) (TFun a b) = over _1 ((x,a):) <$> extractArgs xs b
extractArgs (_:_) _           = errorTC $ ErrGeneral $ text "too many arguments for function"

checkExp :: Exp -> TC CuMinErrCtx Type
checkExp e = local (errContext.userCtx.errExp %~ (e:)) $ go e where
  go (EVar vname) = view (localScope.at vname) >>= \case
    Nothing -> errorTC (ErrVarNotInScope vname)
    Just ty -> return ty

  go (EFun fn tyArgs) = use (topScope.at fn) >>= \case
    Nothing -> errorTC (ErrFunNotInScope fn)
    Just decl@(TyDecl tyDeclVars tyConstraints _) -> do
      mapM_ checkType tyArgs
      let
        dataConstraints = tyConstraints >>= \(TyConstraint tyClass tv) -> if tyClass == "Data" then return tv else []
        dataConstraintsIndices = dataConstraints >>= (`elemIndices` tyDeclVars)
      mapM_ (checkForDataInstance . (tyArgs !!)) dataConstraintsIndices
      instantiate tyArgs decl

  go (EApp callee arg) = do
    argTy    <- checkExp arg
    checkExp callee >>= \case
      TFun funArg funDest -> do
        assertTypesEq funArg argTy
        return funDest
      calleeTy -> errorTC $ ErrTypeMismatch calleeTy (TFun argTy (TVar "<result>"))

  go (ELet var e body) = do
    varTy <- checkExp e
    local (localScope.at var .~ Just varTy) $ checkExp body

  go (ELetFree var ty body) = do
    checkForDataInstance ty
    local (localScope.at var .~ Just ty) $ checkExp body

  go (ELit (LNat _)) = return TNat

  go (EPrim PrimAdd [x, y]) = do
    checkExp x >>= assertTypesEq TNat
    checkExp y >>= assertTypesEq TNat
    return TNat

  go (EPrim PrimEq [x, y]) = do
    checkExp x >>= assertTypesEq TNat
    checkExp y >>= assertTypesEq TNat
    return (TCon "Bool" [])

  go e@(EPrim _ _) = errorTC $ ErrGeneral $ text $ "Wrong use of primitive operation: " ++ show e

  go (ECon con tyArgs) = use (topScope.at con) >>= \case
    Nothing -> errorTC (ErrConNotInScope con)
    Just decl -> do
       mapM_ checkType tyArgs
       instantiate tyArgs decl

  go (ECase expr alts) = do
    expTy  <- checkExp expr
    mapM (checkAlt expTy) alts >>= \case
      [] -> errorTC $ ErrGeneral $ text "case expression without alternatives"
      (aty:atys) -> case find (/=aty) atys of
        Nothing -> return aty
        Just wrongTy -> errorTC $ ErrTypeMismatch aty wrongTy

  go (EFailed ty) = return ty

checkAlt :: Type -> Alt -> TC CuMinErrCtx Type
checkAlt pty (Alt pat body) = case pat of
  PVar v -> local (localScope.at v .~ Just pty) $ checkExp body
  PCon c vs -> case pty of
    TVar _        -> errorTC $ ErrGeneral $ text "cannot pattern match on unknown type"
    TCon _ tyArgs -> use (topScope.at c) >>= \case
      Nothing   -> errorTC $ ErrConNotInScope c
      Just decl -> do
        conTy <- instantiate tyArgs decl
        let (argTys, retTy) = dissectFunTy conTy
        when (length argTys /= length vs) (errorTC $ ErrGeneral $ text "wrong number of arguments in pattern")
        when (retTy /= pty) (errorTC $ ErrTypeMismatch retTy pty)
        local (localScope %~ M.union (M.fromList $ zip vs argTys)) $ checkExp body
