{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
module FunLogic.Core.TypeChecker where

import           Control.Applicative
import           Control.Lens
import           Control.Monad                hiding (mapM, mapM_, forM_)
import           Control.Monad.Error          hiding (mapM, mapM_, forM_)
import           Control.Monad.Reader         hiding (mapM, mapM_, forM_)
import           Control.Monad.RWS            hiding (mapM, mapM_, forM_)
import           Control.Monad.State          hiding (mapM, mapM_, forM_)
import           Data.Default.Class
import           Data.Foldable
import           Data.List (elemIndex)
import qualified Data.HashSet                 as HS
import qualified Data.Map                     as M
import           Data.Maybe
import qualified Data.Set                     as S
import           Prelude                      hiding (any, concat, foldr, mapM,
                                               mapM_, elem)
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import           FunLogic.Core.AST
import           FunLogic.Core.Pretty

type VarId = Int

-- | Errors reported by type checker
data TCErr e
  = TCErr ErrMsg (ErrCtx e)
  deriving (Show)

-- | Context of an error message.
data ErrCtx e
  = ErrCtx
  { _errSrc  :: Maybe SrcRef  -- ^ position in the source file of the errorneous code part
  , _errType :: Maybe Type    -- ^ type signature currently being checkd
  , _userCtx :: e             -- ^ some other context information
  } deriving (Show)

-- | Possible type checker errors
data ErrMsg
  = ErrGeneral Doc
  | ErrVarNotInScope Name
  | ErrTyVarNotInScope TVName
  | ErrTyConNotInScope Name
  | ErrConNotInScope Name
  | ErrFunNotInScope Name
  | ErrKindMismatch Type Kind Kind
  | ErrFreeVarInDecl TyDecl TVName
  | ErrTypeMismatch Type Type
  | ErrNoDataInstanceForTyCon Name [Type]
  | ErrNoDataConstraintForTyVar TVName [Type]
  deriving (Show)

-- | Kind of a type constructor.
-- Currently only represented by arity, since higher kinded types are not supported.
data Kind
  = Kind
  { _kindArity :: Int
  } deriving (Show, Eq)

instance Default e => Error (TCErr e) where
  noMsg = strMsg "unknown error in type checker"
  strMsg msg = TCErr (ErrGeneral $ text msg) def

instance Default e => Default (ErrCtx e) where
  def = ErrCtx Nothing Nothing def

makeLenses ''ErrCtx

-- * Pretty printing of error messages

instance Pretty e => Pretty (ErrCtx e) where
  pretty (ErrCtx src ty user)
    = text "Location:" <+> highlight (text $ maybe "unknown" show src)
    <> maybe mempty (\t -> line <> text "Type:" <+> highlight (prettyType t)) ty
    PP.<$> pretty user

instance Pretty e => Pretty (TCErr e) where
  pretty = prettyErr

instance Pretty ErrMsg where
  pretty = prettyMsg

errorDoc :: Doc -> Doc
errorDoc = red

highlight :: Doc -> Doc
highlight = dullyellow

prettyErr :: Pretty e => TCErr e -> Doc
prettyErr (TCErr msg ctx) = errorDoc (text "Error!") PP.<$> indent 2 (prettyMsg msg)
  PP.<$> errorDoc (text "Context:") PP.<$> indent 2 (pretty ctx)

prettyMsg :: ErrMsg -> Doc
prettyMsg err = case err of
    ErrGeneral doc -> doc
    ErrVarNotInScope var -> notInScope "variable" var
    ErrTyVarNotInScope var -> notInScope "type variable" var
    ErrTyConNotInScope con -> notInScope "type constructor" con
    ErrConNotInScope con -> notInScope "data constructor" con
    ErrFunNotInScope fun -> notInScope "function" fun
    ErrKindMismatch ty (Kind n1) (Kind n2) -> text "Kind mismatch for" <+> prettyType ty PP.<$> indent 2
      (      highlight (text "Expected:") <+> int n1 <+> pluralize n1 "argument"
      PP.<$> highlight (text "Given:   ") <+> int n2 <+> pluralize n2 "argument")
    ErrFreeVarInDecl tydec v ->
          text "The type " <+> highlight (prettyTyDecl tydec)
      <+> text "contains a free type variable" <+> highlight (text v)
    ErrTypeMismatch expected actual ->
             highlight (text "Expected type:") <+> prettyType expected
      PP.<$> highlight (text "Actual type:  ") <+> prettyType actual
    ErrNoDataInstanceForTyCon tyCon tys ->
             hang 2 $ highlight (text "type constructor") <+> text tyCon <+> highlight (text "has no Data instance") <>
             foldr (\ty d -> d </> (highlight (text "required by type:") <+> prettyType ty)) PP.empty tys
    ErrNoDataConstraintForTyVar tv tys ->
             hang 2 $ highlight (text "type variable") <+> text tv <+> highlight (text "has no Data constraint") <>
             foldr (\ty d -> d </> (highlight (text "required by type:") <+> prettyType ty)) PP.empty tys
  where
    notInScope x y = text x <+> highlight (text y) <+> text "not in scope!"

pluralize :: (Num a, Ord a) => a -> String -> Doc
pluralize 1 str = text str
pluralize _ str = text $ str ++ "s"

-- * Definition of type checker monad

-- | State of the type checker.
data TCState
  = TCState
    { _topScope  :: M.Map Name TyDecl -- ^ available top level function definitions
    , _typeScope :: M.Map Name Kind   -- ^ type constructors in scope
    , _dataScope :: M.Map Name (S.Set Int) -- ^ tyCon -> {tyVarIdx} (the type vars that need to be in Data)
    } deriving (Eq, Show)

-- | Environment used during type checking
data TCEnv e
  = TCEnv
    { _localScope :: M.Map Name Type   -- ^ identifiers and their corresponding types currently in scope
    , _tyVarScope :: HS.HashSet Name   -- ^ type variables currently in scope
    , _errContext :: ErrCtx e
    , _localConstraints :: [TyConstraint] -- ^ type constraints from a top-level binding
    }

instance Default TCState where
  def = TCState M.empty M.empty M.empty

instance Default e => Default (TCEnv e) where
  def = TCEnv M.empty HS.empty def []

type MonadTCErr e = MonadError (TCErr e)
type MonadTCReader e = MonadReader (TCEnv e)

-- | Type checker monad.
newtype TC e a = TC { unwrapTC :: ErrorT (TCErr e) (RWS (TCEnv e) () TCState) a }
             deriving (Functor, Applicative, Monad, MonadTCErr e, MonadTCReader e, MonadState TCState)

makeLenses ''TCEnv
makeLenses ''TCState

-- * Type checker interface

-- | Run a type checker action with a given initial state and environment.
evalTC :: TC e a -> TCState -> TCEnv e -> Either (TCErr e) a
evalTC action istate env = fst $ evalRWS (runErrorT (unwrapTC action)) env istate

errorTC :: Default e => ErrMsg -> TC e a
errorTC msg = view errContext >>= throwError . TCErr msg

catchTC :: Default e => TC e a -> (TCErr e -> TC e a) -> TC e a
catchTC action handler = TC $ unwrapTC action `catchError` (unwrapTC . handler)

assertTypesEq :: Default e => Type -> Type -> TC e ()
assertTypesEq ty1 ty2 = when (ty1 /= ty2) (errorTC $ ErrTypeMismatch ty1 ty2)

-- * Helper Functions

modifyM :: (MonadState s m) => Lens' s a -> (a -> m a) -> m ()
modifyM lns f = use lns >>= f >>= assign lns

readOnly :: (Monad m) => ReaderT s m a -> StateT s m a
readOnly action = get >>= lift . runReaderT action

withTyVars :: Default e => [TVName] -> TC e a -> TC e a
withTyVars tvs = local (tyVarScope %~ HS.union (HS.fromList tvs))

-- | Returns the kind of an ADT type constructor
adtKind :: ADT -> Kind
adtKind adt = adt^.adtTyArgs.to length.to Kind

-- | Instantiates type variables in a type declaration
--   without checking the context.
instantiate :: Default e => [Type] -> TyDecl -> TC e Type
instantiate tyArgs decl@(TyDecl tyVars _ ty) = do
  when (length tyArgs /= length tyVars) $ errorTC (ErrGeneral $ hang 2 $ text "Wrong number of arguments for type instantiation of" </> highlight (prettyTyDecl decl))
  let
    subst = M.fromList $ zip tyVars tyArgs
    replaceVar (TVar v) = case M.lookup v subst of
      Nothing -> errorTC (ErrFreeVarInDecl decl v)
      Just x -> return x
    replaceVar x = return x
  transformM replaceVar ty

-- * Type Checking

-- | Extends the scope with all definitions contained in the module, but does not check them.
-- Only use this function if you know that the module that is passed to it has been type-checked previously.
unsafeIncludeModule :: (Default e, IsBinding b) => CoreModule b -> TC e ()
unsafeIncludeModule cuminMod = do
  typeScope %= M.union (adtKind <$> cuminMod^.modADTs)
  topScope  %= M.union (view bindingType <$> cuminMod^.modBinds)
  topScope  %= M.union (M.unions $ map adtConstructorTypes $ M.elems $ cuminMod^.modADTs)

-- | Typechecks an ADT
checkADT :: Default e => ADT -> TC e ()
checkADT adt = local (errContext.errSrc .~ Just (adt^.adtSrcRef))
    $ withTyVars (adt^.adtTyArgs)
    $ mapMOf_ (adtConstr.traverse) checkConstr adt
  where checkConstr (ConDecl _ args) = mapM_ checkType args

checkType :: Default e => Type -> TC e ()
checkType ty = local (errContext.errType .~ Just ty) $ go ty where
  go :: Default e => Type -> TC e ()
  go (TVar v) = views tyVarScope (HS.member v) >>= flip unless (errorTC $ ErrTyVarNotInScope v)
  go t@(TCon tcon args) =
    use (typeScope . at tcon) >>= \case
      Nothing  -> errorTC (ErrTyConNotInScope tcon)
      Just (Kind nargs)
        | nargs == length args -> mapM_ go args
        | otherwise -> errorTC $ ErrKindMismatch t (Kind nargs) (Kind $ length args)

checkForDataInstance :: Default e => Type -> TC e ()
checkForDataInstance (TVar tv) = do
    constraints <- view localConstraints
    unless (TyConstraint "Data" tv `elem` constraints) $ errorTC $ ErrNoDataConstraintForTyVar tv []
checkForDataInstance ty@(TCon tyCon tys) =
  checkRecursively `catchTC` \(TCErr msg _) -> case msg of
    ErrNoDataInstanceForTyCon tc ts -> errorTC  $ ErrNoDataInstanceForTyCon tc $ ty:ts
    ErrNoDataConstraintForTyVar tv ts -> errorTC  $ ErrNoDataConstraintForTyVar tv $ ty:ts
    e -> errorTC e
  where
  checkRecursively = do
    constraints <- use $ dataScope.at tyCon
    case constraints of
      Nothing -> errorTC $ ErrNoDataInstanceForTyCon tyCon []
      Just dataIndices -> mapM_ (checkForDataInstance . snd) $ filter ((`elem` dataIndices) . fst) $ zip [0..] tys

-- * Data deriving

-- | Given ADTs, derives their data instances
deriveDataInstances :: M.Map Name ADT -> M.Map Name (S.Set Int)
deriveDataInstances adts = flip execState M.empty $ do
  put $ const S.empty <$> adts
  fixpointIteration (itraverse_ addConstraints adts) -- iteratively tighten the constraints until fixpoint is reached

-- | Add Data constraints for relevant type variables
-- that can be determined by looking only one layer deep
-- into the definition of the ADT.
--
-- When this function is iterated for all type constructors,
-- it will eventually have determined all necessary constraints.
-- When a fixpoint is reached, they are also sufficient.
-- Such a fixpoint will be reached because the definition of an
-- ADT and thus also the nesting level is finite.
--
-- EXAMPLE: Given the ADTs
-- data Phantom a = Phantom
-- data Rec1 a b = R1 a (Rec2 a b)
-- data Rec2 a b = R2 b (Rec1 b a)
--
-- 1st iteration:
-- Phantom: {} ("a" does not occur on RHS)
-- Rec1: {0}, i.e. the 0th type var, i.e. {"a"}
-- Rec2: {1}, i.e. {"b"}
--
-- 2nd iteration:
-- Phantom: {} (unchanged)
-- Rec1: {0,1}, i.e. {"a", "b"} ("b" has been added
--   since it is required by Rec2 since iteration 1)
-- Rec2: {1}, i.e. {"b"} ("Data b" is required by
--   Rec1, but this does not change anything)
--
-- 3rd iteration:
-- Phantom: {} (unchanged)
-- Rec1: {0,1}, i.e. {"a", "b"} (unchanged)
-- Rec2: {0, 1}, i.e. {"a", "b"} ("Data a" is required
--   by Rec1 since iteration 2)
--
-- Afterwards: no more changes.
addConstraints :: String -> ADT -> State (M.Map Name (S.Set Int)) ()
addConstraints name adt = forM_ (adt^.adtConstr) $
  \(ConDecl _ tys) -> mapM_ requireDataForType tys
  where
    -- collect type vars directly
    requireDataForType (TVar tv) =
      let
        tyVarIdx = fromMaybe (error
          "Internal error in Salt type checker while deriving data instances: \
          \type variable doesn't occur on the left-hand side of the definition.") $
          tv `elemIndex` (adt^.adtTyArgs)
      in at name %= fmap (S.insert tyVarIdx) -- add Data constraint for this type variable
    -- for type constructors, recursively check the types passed to the type constructor that require Data
    requireDataForType (TCon tyCon tys) = do
      tyConConstraints <- use (at tyCon)
      case tyConConstraints of
        Just dataIndices -> traverse_ (requireDataForType . (tys !!)) dataIndices
        Nothing -> at name .= Nothing -- no Data instance allowed in this case

-- | Repeat the monadic action until the state does not change anymore.
fixpointIteration :: (MonadState s m, Eq s) => m () -> m ()
fixpointIteration step = do
  old <- get
  step
  new <- get
  when (old /= new) $ fixpointIteration step
