{-# LANGUAGE PatternSynonyms, TemplateHaskell #-}
module Language.SaLT.AST where

import Control.Lens
import qualified Data.Map as M
import Data.Text

type Column = Int
type Row    = Int

data SrcRef
  = SrcRef
    { _srcFile  :: FilePath
    , _srcBegin :: (Row, Column)
    , _srcEnd   :: (Row, Column)
    } deriving (Show, Eq, Ord)

type Name = String

data Module = Module
  { _modName   :: Name
  , _modBinds  :: M.Map Name Binding
  , _modADTs   :: M.Map Name ADT
  , _modConstr :: M.Map Name TyDecl
  } deriving (Show)

data Binding = Binding
  { _bindingExpr :: Exp
  , _bindingType :: TyDecl
  , _bindingSrc  :: SrcRef
  } deriving (Show)

data ADT = ADT
  { _adtName   :: Name
  , _adtTyArgs :: [TVName]
  , _adtConstr :: [ConDecl]
  , _adtSrcRef :: SrcRef
  } deriving (Show)

data Decl
  = DTop Name Binding
  | DData ADT
  deriving (Show)

data ConDecl
  = ConDecl Name [Type]
  deriving (Show)

data TVName
  = RName String
  | UName String Int
  deriving (Show, Eq, Ord)

-- | Example: `forall a.Data a => (a,a)`
--   --> `TyDecl [RName "a"] [TyConstraint "Data" (RName "a")] (TCon "Pair" [(RName "a"), (RName "a")])`
data TyDecl
  = TyDecl [TVName] [TyConstraint] Type
  deriving (Show)

-- | Example: `Data a` --> `TyConstraint "Data" "a"`
data TyConstraint
  = TyConstraint Name TVName
  deriving (Show)

data Type
  = TVar  TVName
  | TCon  Name [Type]
  deriving (Show)

-- Useful Type Pattern Synonyms
pattern TFun x y = TCon "->" [x, y]
pattern TSet x   = TCon "Set" [x]
pattern TTup x y = TCon "Pair" [x, y]

data Exp
  = EVar Name
  | EFun Name [Type]
  | ELam Name Type Exp
  | EApp Exp Exp
  | ELit Lit
  | EPrim PrimOp [Exp]
  | ECon Name [Type] [Exp]
  | EPair Exp Exp
  | ESet Exp
  | ECase Exp [Alt]
  | EFail Type
  | EAny Type
  deriving (Show)

data Lit
  = LInt Integer
  deriving (Show)

data PrimOp
  = PrimAdd
  | PrimEq
  | PrimBind
  deriving (Show)

data Alt
  = Alt Pat Exp
  deriving (Show)

data Pat
  = PCon Name [Name]
  | PTup [Name]
  | PVar Name
  deriving (Show)

-- Lenses
makeLenses ''Module
makeLenses ''Binding
makeLenses ''ADT
