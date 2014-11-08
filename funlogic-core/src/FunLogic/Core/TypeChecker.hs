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
import qualified Data.Map                     as M
import           Data.Maybe
import           Data.Traversable
import           Prelude                      hiding (any, concat, foldr, mapM,
                                               mapM_)
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import           FunLogic.Core.AST

type VarId = Int

-- | Errors reported by type checker
data TCErr
  = TCErr ErrMsg ErrCtx
  deriving (Show)

data ErrMsg
  = ErrGeneral String
  | ErrVarNotInScope Name
  | ErrTyConNotInScope Name
  | ErrConNotInScope Name
  | ErrFunNotInScope Name
  | ErrKindMismatch Type Kind Kind
  | ErrFreeVarInDecl TyDecl Name
  | ErrTypeMismatch Type Type
  deriving (Show)

data ErrCtx
  = ErrCtx
  { _errSrc :: Maybe SrcRef
  } deriving (Show)

data Kind
  = KStar
  | KFun Kind Kind
  deriving (Show, Eq)

instance Error TCErr where
  noMsg = strMsg "unknown error in type checker"
  strMsg msg = TCErr (ErrGeneral msg) (ErrCtx Nothing)

makeLenses ''ErrCtx

prettyErr :: TCErr -> Doc
prettyErr (TCErr msg ctx) = red (text "Error!") <+> fromMaybe mempty (text . show <$> ctx^.errSrc)
  PP.<$> indent 2 (prettyMsg msg)

prettyMsg :: ErrMsg -> Doc
prettyMsg err = case err of
    ErrGeneral str -> text str
    ErrVarNotInScope var -> notInScope "variable" var
    ErrTyConNotInScope con -> notInScope "type constructor" con
    ErrConNotInScope con -> notInScope "data constructor" con
    ErrFunNotInScope fun -> notInScope "function" fun
    ErrKindMismatch ty k1 k2 -> text "Kind mismatch for" <+> text (show ty) PP.<$> indent 2
      (      dullyellow (text "Expected:") <+> text (show k1)
      PP.<$> dullyellow (text "Actual:  ") <+> text (show k2))
    ErrFreeVarInDecl ty v ->
          text "The type " <+> dullyellow (text $ show ty)
      <+> text "contains a free type variable" <+> dullyellow (text v)
    ErrTypeMismatch expected actual ->
             dullyellow (text "Expected type:") <+> text (show expected)
      PP.<$> dullyellow (text "Actual type:  ") <+> text (show actual)
  where
    notInScope x y = text x <+> dullyellow (text y) <+> text "not in scope!"


-- | State of the type checker.
data TCState
  = TCState
    { _topScope  :: M.Map Name TyDecl -- ^ available top level function definitions
    , _typeScope :: M.Map Name Kind   -- ^ type constructors in scope
    }

-- | Environment used during type checking
data TCEnv
  = TCEnv
    { _localScope :: M.Map Name Type   -- ^ identifiers and their corresponding types currently in scope
    , _errContext :: ErrCtx
    }

instance Default ErrCtx where
  def = ErrCtx Nothing

instance Default TCState where
  def = TCState M.empty M.empty

instance Default TCEnv where
  def = TCEnv M.empty def

-- | Type checker monad.
newtype TC a = TC { unwrapTC :: ErrorT TCErr (RWS TCEnv () TCState) a }
             deriving (Functor, Applicative, Monad, MonadError TCErr, MonadReader TCEnv, MonadState TCState)

makeLenses ''TCEnv
makeLenses ''TCState

-- * Type checker interface

-- | Run a type checker action with a given initial state and environment.
evalTC :: TC a -> TCState -> TCEnv -> Either TCErr a
evalTC action istate env = fst $ evalRWS (runErrorT (unwrapTC action)) env istate

errorTC :: ErrMsg -> TC a
errorTC msg = view errContext >>= throwError . TCErr msg

assertTypesEq :: Type -> Type -> TC ()
assertTypesEq ty1 ty2 = when (ty1 /= ty2) (errorTC $ ErrTypeMismatch ty1 ty2)

-- * Helper Functions

modifyM :: (MonadState s m) => Lens' s a -> (a -> m a) -> m ()
modifyM lns f = use lns >>= f >>= assign lns

readOnly :: (Monad m) => ReaderT s m a -> StateT s m a
readOnly action = get >>= lift . runReaderT action

-- | Returns the kind of an ADT type constructor
adtKind :: ADT -> Kind
adtKind adt = foldr KFun KStar (KStar <$ adt ^. adtTyArgs)

-- | Instantiates type variables in a type declaration
instantiate :: [Type] -> TyDecl -> TC Type
instantiate tyArgs decl@(TyDecl tyVars ctx ty) = do
  when (length tyArgs /= length tyVars) $ errorTC (ErrGeneral $ "Wrong number of arguments for type instantiation of: " ++ show decl)
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
checkADT :: ADT -> TC ()
checkADT adt = local (errContext.errSrc .~ Just (adt^.adtSrcRef))
             $ mapMOf_ (adtConstr.traverse) checkConstr adt
  where checkConstr (ConDecl _ args) = mapM_ checkKind args

-- | Verifies that a type is of the right kind
checkKind :: Type -> TC Kind
checkKind (TVar _) = return KStar -- type variables currently always have kind *
checkKind t@(TCon tcon args) = do
  argKinds   <- mapM checkKind args
  let actual = foldr KFun KStar argKinds
  use (typeScope . at tcon) >>= \case
    Nothing  -> errorTC (ErrTyConNotInScope tcon)
    Just expected
      | expected == actual -> return KStar
      | otherwise -> errorTC (ErrKindMismatch t expected actual)
