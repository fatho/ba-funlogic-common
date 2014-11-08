{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.CuMin.AST
    ( Module(..)
    , Binding(..)
    , Decl(..)
    , module FunLogic.Core.AST
    , Exp(..)
    , Lit(..)
    , PrimOp(..)
    , Alt(..)
    , Pat(..)
    , modName, modADTs, modBinds
    , bindingName, bindingArgs, bindingExpr, bindingType, bindingSrc
    ) where

import           Control.Lens
import qualified Data.Map          as M

import           FunLogic.Core.AST

data Module = Module
  { _modName  :: Name
  , _modBinds :: M.Map Name Binding
  , _modADTs  :: M.Map Name ADT
  } deriving (Show)

data Binding = Binding
  { _bindingName :: Name
  , _bindingArgs :: [Name]
  , _bindingExpr :: Exp
  , _bindingType :: TyDecl
  , _bindingSrc  :: SrcRef
  } deriving (Show)

data Decl
  = DTop Binding
  | DData ADT
  deriving (Show)

data Exp
  = EVar Name
  | ELet Name Exp Exp
  | ELetFree Name Type Exp
  | EFailed Type
  | EFun Name [Type]
  | EApp Exp Exp
  | ELit Lit
  | EPrim PrimOp [Exp]
  | ECon Name [Type]
  | ECase Exp [Alt]
  deriving (Show)

data Lit
  = LInt Integer
  deriving (Show)

data PrimOp
  = PrimAdd
  | PrimEq
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

