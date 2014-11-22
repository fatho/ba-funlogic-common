{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
module Language.SaLT.TypeChecker where

import           Control.Applicative
import           Control.Lens
import           Control.Monad                hiding (mapM, mapM_, forM_)
import           Control.Monad.Reader         hiding (mapM, mapM_, forM_)
import           Data.Default.Class
import           Data.Foldable
import           Data.List (elemIndices)
import qualified Data.Map                     as M
import           Data.Monoid
import qualified Data.Set                     as S
import           Data.Traversable
import           Prelude                      hiding (any, foldr, mapM, mapM_, elem)
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>),(<>))

import           FunLogic.Core.TypeChecker
import           Language.SaLT.AST
import           Language.SaLT.Pretty

-- * Built-Int types

pattern TSet x = TCon "Set" [x]

builtInTyCons :: M.Map Name Kind
builtInTyCons = M.fromList
  [ ("Set", Kind 1)
  , ("Nat", Kind 0)
  , ("->", Kind 2)
  ]

builtInADTs :: M.Map Name ADT
builtInADTs = M.fromList $ map (\adt -> (adt^.adtName, adt)) [adtDefBool, adtDefList, adtDefPair]

builtInDataInstances :: M.Map Name (S.Set Int)
builtInDataInstances = M.fromList
  [ ("Nat", S.empty)
  , ("Bool", S.empty)
  , ("List", S.fromList [0])
  , ("Pair", S.fromList [0,1])
  ]

-- * Error Types

data SaltErrCtx
  = SaltErrCtx
  { _errExp :: [Exp]
  }

makeLenses ''SaltErrCtx

instance Default SaltErrCtx where
  def = SaltErrCtx []

instance Pretty SaltErrCtx where
  pretty (SaltErrCtx ectx)
    | null ectx = mempty
    | otherwise = foldMap (\e -> highlight (text "in") <+> align (prettyExp e) <> line) ectx

-- * Type Checking

includeBuiltIns :: TC SaltErrCtx ()
includeBuiltIns = do
  typeScope %= M.union builtInTyCons
  typeScope %= M.union (adtKind <$> builtInADTs)
  topScope  %= M.union (M.unions $ map adtConstructorTypes $ M.elems builtInADTs)

-- | Typechecks a module.
checkModule :: Module -> TC SaltErrCtx ()
checkModule saltMod = do
  -- check kinds of ADT definitions
  typeScope %= M.union (adtKind <$> saltMod^.modADTs)
  mapM_ checkADT (saltMod^.modADTs)
  -- derive Data instances
  dataScope %= M.union builtInDataInstances
  deriveDataInstances (saltMod^.modADTs)
  -- check all top level bindings
  topScope %= M.union (view bindingType <$> saltMod^.modBinds)
  topScope %= M.union (M.unions $ map adtConstructorTypes $ M.elems $ saltMod^.modADTs)
  mapM_ checkBinding (saltMod^.modBinds)

-- | Typechecks a single top level binding
checkBinding :: Binding -> TC SaltErrCtx ()
checkBinding bnd = let (TyDecl tvars tconstraints ty) = bnd^.bindingType in
  local ( (errContext.errSrc .~ Just (bnd^.bindingSrc))
        . (localConstraints .~ tconstraints)) $
  withTyVars tvars $ do
    checkType ty
    bodyTy <- checkExp $ bnd^.bindingExpr
    assertTypesEq bodyTy ty

checkExp :: Exp -> TC SaltErrCtx Type
checkExp e = local (errContext.userCtx.errExp %~ (e:)) $ go e where
  go (EVar vname) = view (localScope.at vname) >>= \case
    Nothing -> use (topScope.at vname) >>= \case
      Nothing -> errorTC (ErrVarNotInScope vname)
      Just (TyDecl args _ ty)
        | null args -> return ty
        | otherwise -> errorTC (ErrVarNotInScope vname)
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

  go (ELam argName argTy body) = do
    checkType argTy
    bodyTy <- local (localScope.at argName .~ Just argTy) (checkExp body)
    return (TFun argTy bodyTy)

  go (EApp callee arg) = do
    argTy    <- checkExp arg
    checkExp callee >>= \case
      TFun funArg funDest -> do
        assertTypesEq funArg argTy
        return funDest
      calleeTy -> errorTC $ ErrTypeMismatch calleeTy (TFun argTy (TVar "<result>"))

  go (ELit (LInt _)) = return TNat

  go (EPrim PrimAdd [x, y]) = do
    checkExp x >>= assertTypesEq TNat
    checkExp y >>= assertTypesEq TNat
    return TNat

  go (EPrim PrimEq [x, y]) = do
    checkExp x >>= assertTypesEq TNat
    checkExp y >>= assertTypesEq TNat
    return (TCon "Bool" [])

  go (EPrim PrimBind [x, y]) = do
    elTy <- checkExp x >>= \case
      TSet elTy -> return elTy
      other  -> errorTC $ ErrTypeMismatch (TSet $ TVar "_") other
    checkExp y >>= \case
       TFun argTy (TSet resElTy)
          | argTy == elTy -> return (TSet resElTy)
       other  -> errorTC $ ErrTypeMismatch (TFun elTy $ TSet $ TVar "_") other

  go (EPrim _ _) = errorTC $ ErrGeneral $ text "Wrong use of primitive operation:" <+> prettyExp e

  go (ECon con tyArgs) = use (topScope.at con) >>= \case
    Nothing -> errorTC (ErrConNotInScope con)
    Just decl ->  do
      mapM_ checkType tyArgs
      instantiate tyArgs decl

  go (ESet el) = TSet <$> checkExp el

  go (ECase expr alts) = do
    expTy  <- checkExp expr
    (aty:atys) <- mapM (checkAlt expTy) alts
    case find (/=aty) atys of
      Nothing -> return aty
      Just wrongTy -> errorTC $ ErrTypeMismatch aty wrongTy

  go (EFailed ty) = checkType ty >> return ty

  go (EUnknown ty) = do
    checkType ty
    checkForDataInstance ty
    return $ TSet ty

checkAlt :: Type -> Alt -> TC SaltErrCtx Type
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
