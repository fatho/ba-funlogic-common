{-# LANGUAGE PatternSynonyms, TemplateHaskell #-}
module Language.SaLT.AST
  ( Module(..)
  , Binding(..)
  , Decl(..)
  , module FunLogic.Core.AST
  , Exp(..)
  , Lit(..)
  , PrimOp(..)
  , Alt(..)
  , Pat(..)
  , modName, modADTs, modBinds, modConstr
  , bindingExpr, bindingType, bindingSrc
  ) where

import Control.Lens
import qualified Data.Map as M

import FunLogic.Core.AST

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

data Decl
  = DTop Name Binding
  | DData ADT
  deriving (Show)

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
  | PVar Name
  deriving (Show)

-- Lenses
makeLenses ''Module
makeLenses ''Binding
