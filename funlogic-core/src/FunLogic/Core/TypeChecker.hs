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
import           Control.Monad                hiding (mapM, mapM_)
import           Control.Monad.Error          hiding (mapM, mapM_)
import           Control.Monad.Reader         hiding (mapM, mapM_)
import           Control.Monad.RWS            hiding (mapM, mapM_)
import           Control.Monad.State          hiding (mapM, mapM_)
import           Data.Default.Class
import           Data.Foldable
import qualified Data.HashSet                 as HS
import qualified Data.Map                     as M
import           Data.Maybe
import           Prelude                      hiding (any, concat, foldr, mapM,
                                               mapM_)
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

instance Pretty e => Pretty (ErrCtx e) where
  pretty (ErrCtx src ty user)
    = text "Location:" <+> text (maybe "unknown" show src)
    <> maybe mempty (\t -> line <> text "Type:" <+> prettyType t) ty
    PP.<$> pretty user

makeLenses ''ErrCtx

-- * Pretty printing of error messages

prettyErr :: Pretty e => TCErr e -> Doc
prettyErr (TCErr msg ctx) = red (text "Error!") PP.<$> indent 2 (prettyMsg msg)
  PP.<$> (text "Context:" PP.<$> indent 2 (pretty ctx))

prettyMsg :: ErrMsg -> Doc
prettyMsg err = case err of
    ErrGeneral doc -> doc
    ErrVarNotInScope var -> notInScope "variable" var
    ErrTyVarNotInScope var -> notInScope "type variable" var
    ErrTyConNotInScope con -> notInScope "type constructor" con
    ErrConNotInScope con -> notInScope "data constructor" con
    ErrFunNotInScope fun -> notInScope "function" fun
    ErrKindMismatch ty (Kind n1) (Kind n2) -> text "Kind mismatch for" <+> prettyType ty PP.<$> indent 2
      (      dullyellow (text "Expected:") <+> int n1 <+> pluralize n1 "argument"
      PP.<$> dullyellow (text "Given:   ") <+> int n2 <+> pluralize n2 "argument")
    ErrFreeVarInDecl tydec v ->
          text "The type " <+> dullyellow (prettyTyDecl tydec)
      <+> text "contains a free type variable" <+> dullyellow (text v)
    ErrTypeMismatch expected actual ->
             dullyellow (text "Expected type:") <+> prettyType expected
      PP.<$> dullyellow (text "Actual type:  ") <+> prettyType actual
  where
    notInScope x y = text x <+> dullyellow (text y) <+> text "not in scope!"

pluralize :: (Num a, Ord a) => a -> String -> Doc
pluralize 1 str = text str
pluralize _ str = text $ str ++ "s"

-- * Definition of type checker monad

-- | State of the type checker.
data TCState
  = TCState
    { _topScope  :: M.Map Name TyDecl -- ^ available top level function definitions
    , _typeScope :: M.Map Name Kind   -- ^ type constructors in scope
    }

-- | Environment used during type checking
data TCEnv e
  = TCEnv
    { _localScope :: M.Map Name Type   -- ^ identifiers and their corresponding types currently in scope
    , _tyVarScope :: HS.HashSet Name   -- ^ type variables currently in scope
    , _errContext :: ErrCtx e
    }

instance Default TCState where
  def = TCState M.empty M.empty

instance Default e => Default (TCEnv e) where
  def = TCEnv M.empty HS.empty def

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
instantiate :: Default e => [Type] -> TyDecl -> TC e Type
instantiate tyArgs decl@(TyDecl tyVars ctx ty) = do
  when (length tyArgs /= length tyVars) $ errorTC (ErrGeneral $ text $ "Wrong number of arguments for type instantiation of: " ++ show decl)
  -- TODO: check context when instantiating type declaration
  let
    subst = M.fromList $ zip tyVars tyArgs
    replaceVar (TVar v) = case M.lookup v subst of
      Nothing -> errorTC (ErrFreeVarInDecl decl v)
      Just x -> return x
    replaceVar x = return x
  transformM replaceVar ty

-- * Type Checking

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
