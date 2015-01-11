{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module FunLogic.Core.AST where

import           Control.Lens
import           Data.Data
import           Data.Foldable (Foldable)
import qualified Data.Map      as M

-- | Denotes a column index in the input file.
type Column = Int
-- | Denotes a row index in the input file.
type Row    = Int

-- | A module containing ADTs and bindings of type b
data CoreModule b = CoreModule
  { _modName  :: ModName
  -- ^ The name of the module.
  , _modBinds :: M.Map BindingName b
  -- ^ The top-level bindings defined in this module.
  , _modADTs  :: M.Map TyConName ADT
  -- ^ The algebraic data types defined in this module.
  } deriving (Show, Data, Typeable, Functor, Foldable, Traversable)

-- | A class of lenses into everything that is a binding.
class IsBinding b where
  -- | The type of expressions the binding binds.
  type BindingExp b :: *
  -- | A lens to the name of the binding. (Must be equal to the name used in the '_modBinds' field of 'CoreModule')
  bindingName :: Lens' b BindingName
  -- | A lens to the expression bound by the binding.
  bindingExpr :: Lens' b (BindingExp b)
  -- | A lens to the source code position of the binding.
  bindingSrc  :: Lens' b SrcRef
  -- | A lens to the type declaration of the binding.
  bindingType :: Lens' b TyDecl

-- | A reference to a source code position.
data SrcRef
  = SrcRef
    { _srcFile  :: FilePath
    -- ^ Path of the source file.
    , _srcBegin :: (Row, Column)
    -- ^ Beginning of the source region.
    , _srcEnd   :: (Row, Column)
    -- ^ End of the source region.
    } deriving (Eq, Ord, Data, Typeable)

instance Show SrcRef where
  show (SrcRef path (r1,c1) (r2,c2)) =
    path ++ ":(" ++ show r1 ++ ":" ++ show c1 ++ " - " ++ show r2 ++ ":" ++ show c2 ++ ")"

{-# DEPRECATED Name "Use the specialized type synonyms for names instead to make type signatures more readable" #-}
-- | Denotes a name of a type constructor, data constructor, variable or top-level definition.
type Name = String

-- | Denotes the name of a module.
type ModName = String
-- | Denotes the name of a type constructor.
type TyConName = String
-- | Denotes the name of a data constructor.
type DataConName = String
-- | Denotes the name of a term variable.
type VarName = String
-- | Denotes the name of a top level binding.
type BindingName = String
-- | Denotes the name of a type class.
type ClassName = String
-- | Denotes the name of a type variable.
type TVName = String

-- | Description of algebraic data types in CuMin and SaLT.
data ADT = ADT
  { _adtName   :: TyConName
  -- ^ The name of the data type. It must begin with an upper case letter.
  , _adtTyArgs :: [TVName]
  -- ^ A list of type parameters.
  , _adtConstr :: [ConDecl]
  -- ^ A list of constructors.
  , _adtSrcRef :: SrcRef
  -- ^ The position of the definition in the source file.
  } deriving (Show, Data, Typeable)

instance Eq ADT where
  a1 == a2 =
    _adtName a1 == _adtName a2 &&
    _adtTyArgs a1 == _adtTyArgs a2 &&
    _adtConstr a1 == _adtConstr a2

-- | Description of an 'ADT' constructor.
data ConDecl
  = ConDecl DataConName [Type]
  -- ^ A constructor consists of a 'Name' and a list of argument types.
  deriving (Eq, Show, Data, Typeable)

-- | A CuMin/SaLT literal.
data Lit
  = LNat Integer
  -- ^ A natural number.
  deriving (Eq, Show, Data, Typeable)

-- | A pattern in a case expression.
data Pat
  = PCon DataConName [VarName]
  -- ^ Matches the constructor with the given name and binds its arguments to the variable names given in the list.
  | PVar VarName
  -- ^ Matches anything and binds the scrutinee to the given name.
  deriving (Eq, Show, Data, Typeable)

-- | Describes a quantified type declaration consisting of a unviersal quantification of type variables,
-- a list of constraints and the type.
data TyDecl
  = TyDecl [TVName] [TyConstraint] Type
  -- ^ > forall a.Data a => Pair a Nat
  -- becomes
  -- > TyDecl ["a"] [TyConstraint "Data" "a"] (TCon "Pair" ["a", TCon "Nat" []])
  deriving (Show, Eq, Data, Typeable)

-- | Describes a type constraint.
data TyConstraint
  = TyConstraint ClassName TVName
  -- ^ Example
  -- The constraint
  -- > Data a
  -- becomes
  -- > TyConstraint "Data" "a"
  deriving (Show, Eq, Data, Typeable)

-- | Representation of CuMin/SaLT types.
data Type
  = TVar  TVName
  -- ^ A type variable.
  | TCon  Name [Type]
  -- ^ A type constructor. Note that the function arrow is just a type constructor with two arguments:
  -- > TCon "->" [from, to]
  deriving (Show, Eq, Data, Typeable)

-- plated lenses
instance Plated Type

-- Lenses
makeLenses ''CoreModule
makeLenses ''ADT

-- * Useful Type Pattern Synonyms
-- | Function type constructor.
pattern TFun x y = TCon "->" [x, y]
-- | Natural number type.
pattern TNat     = TCon "Nat" []
-- | Pair type.
pattern TTup x y = TCon "Pair" [x, y]
-- | List type.
pattern TList x  = TCon "List" [x]


-- * Precedence Handling (when to use parentheses in pretty-printer)

-- | Precedence is represented as an integer. Greater numbers mean higher precedence.
type Prec = Int

-- | Class to give syntax elements a precedence for pretty printing.
class HasPrecedence a where
  -- | Assigns each syntax element a precedence.
  prec :: a -> Prec

instance HasPrecedence Type where
  prec = \case
    TFun _ _ -> 1
    -- "Normal" typec constructors have higher precedence than function types
    TCon _ (_:_) -> tyConPrec
    -- these two have highest precedence because they never need parentheses:
    TCon _ [] -> maxTypePrec
    TVar _ -> maxTypePrec

-- | Precendence of type constructors.
tyConPrec :: Prec
tyConPrec = 2

-- | Maximal precedence of type related syntax elements.
maxTypePrec :: Prec
maxTypePrec = 3

-- * Useful Functions

-- | SrcRef value for built-in definitions
srcBuiltIn :: SrcRef
srcBuiltIn = SrcRef "<builtin>" (0,0) (0,0)

-- | Returns the type declarations of the ADT-constructors.
adtConstructorTypes :: ADT -> M.Map Name TyDecl
adtConstructorTypes ADT{..} = M.fromList $ map go _adtConstr where
  go (ConDecl name args) =
    let conTy = foldr TFun (TCon _adtName (map TVar _adtTyArgs)) args
    in (name, TyDecl _adtTyArgs [] conTy)

-- | Splits a function type into a list of argument types and a result type.
dissectFunTy :: Type -> ([Type], Type)
dissectFunTy (TFun x y) = dissectFunTy y & _1 %~ (x:)
dissectFunTy x          = ([], x)

-- | For a list of ADTs, returns a list of constructors that are defined multiple times.
-- `(x, [("Foo", y)])` means that the constructor "Foo" of ADT "x" has previously been defined in ADT y.
findDuplCon :: [ADT] -> [(ADT,[(Name,ADT)])]
findDuplCon xs = go M.empty xs where
  go _ [] = []
  go conset (adt:adts) =
    let adtConSet = M.fromList $ adt^..adtConstr.traverse.to (\(ConDecl n _) -> (n,adt))
        dupCons   = M.toList (M.intersection conset adtConSet)
        otherDups = go (M.union conset adtConSet) adts
    in [(adt,dupCons) | not (null dupCons) ] ++ otherDups

