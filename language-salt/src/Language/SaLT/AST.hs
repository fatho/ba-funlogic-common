{-# LANGUAGE PatternSynonyms #-}
module Language.SaLT.AST where

import Data.Text

type Name = String

type Column = Int
type Row    = Int

data Program = Program [Decl] deriving (Show)

data Decl
  = DTop Name TyDecl Exp
  | DData Name [Name] [ConDecl]
  deriving (Show)

data ConDecl
  = ConDecl Name [Type]
  deriving (Show)

-- | Example: `forall a.Data a => (a,a)`
--   --> `TyDecl ["a"] [TyConstraint "Data" "a"] (TCon "(,)" ["a", "a"])`
data TyDecl
  = TyDecl [Name] [TyConstraint] Type
  deriving (Show)

-- | Example: `Data a` --> `TyConstraint "Data" "a"`
data TyConstraint
  = TyConstraint Name Name
  deriving (Show)

data Type
  = TVar  Name
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
