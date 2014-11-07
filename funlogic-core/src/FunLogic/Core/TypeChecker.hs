{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE StandaloneDeriving         #-}
module FunLogic.Core.TypeChecker where

import           Control.Applicative
import           Control.Lens
import           Control.Monad        hiding (mapM, mapM_)
import           Control.Monad.Error  hiding (mapM, mapM_)
import           Control.Monad.Reader hiding (mapM, mapM_)
import           Control.Monad.RWS    hiding (mapM, mapM_)
import           Control.Monad.State  hiding (mapM, mapM_)
import           Data.Foldable
import qualified Data.Map             as M
import           Data.Traversable
import           Prelude              hiding (foldr, mapM, mapM_, any, concat)

import           FunLogic.Core.AST

type VarId = Int

-- | Errors reported by type checker
data TCErr
  = TCErr ErrMsg
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

data Kind
  = KStar
  | KFun Kind Kind
  deriving (Show, Eq)

instance Error TCErr where
  noMsg = strMsg "unknown error in type checker"
  strMsg msg = TCErr (ErrGeneral msg)

-- | State of the type checker.
data TCState
  = TCState
    { _topScope   :: M.Map Name TyDecl -- ^ available top level function definitions
    , _typeScope  :: M.Map Name Kind   -- ^ type constructors in scope
    }

-- | Environment used during type checking
data TCEnv
  = TCEnv
    { _localScope :: M.Map Name Type   -- ^ identifiers and their corresponding types currently in scope
    }

-- | Type checker monad.
newtype TC a = TC { unwrapTC :: ErrorT TCErr (RWS TCEnv () TCState) a }
             deriving (Functor, Applicative, Monad, MonadError TCErr, MonadReader TCEnv, MonadState TCState)

makeLenses ''TCEnv
makeLenses ''TCState

-- * Type checker interface

-- | Run a type checker action with a given initial state and environment.
evalTC :: TC a -> TCState -> TCEnv -> Either TCErr a
evalTC action istate env = fst $ evalRWS (runErrorT (unwrapTC action)) env istate

errorTC :: MonadError TCErr m => ErrMsg -> m a
errorTC msg = throwError (TCErr msg)

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

-- | Instanciates
instanciate :: [Type] -> TyDecl -> TC Type
instanciate tyArgs decl@(TyDecl tyVars ctx ty) = do
  when (length tyArgs /= length tyVars) $ errorTC (ErrGeneral $ "Wrong number of arguments for type instanciation of: " ++ show decl)
  -- TODO: check context when instanciating type declaration
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
checkADT = mapMOf_ (adtConstr.traverse) checkConstr where
  checkConstr (ConDecl _ args) = mapM_ checkKind args

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
