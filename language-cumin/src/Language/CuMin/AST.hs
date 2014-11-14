{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
module Language.CuMin.AST
    ( Module
    , Binding(..)
    , Decl(..)
    , module FunLogic.Core.AST
    , Exp(..)
    , Lit(..)
    , PrimOp(..)
    , Alt(..)
    , Pat(..)
    , bindingArgs
    ) where

import           Control.Applicative
import           Control.Lens

import           FunLogic.Core.AST

type Module = CoreModule Binding

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
makeLensesFor [("_bindingArgs", "bindingArgs")] ''Binding

instance IsBinding Binding where
  type BindingExp Binding = Exp
  bindingName f bnd = (\x -> bnd {_bindingName = x}) <$> f (_bindingName bnd)
  bindingExpr f bnd = (\x -> bnd {_bindingExpr = x}) <$> f (_bindingExpr bnd)
  bindingType f bnd = (\x -> bnd {_bindingType = x}) <$> f (_bindingType bnd)
  bindingSrc  f bnd = (\x -> bnd {_bindingSrc  = x}) <$> f (_bindingSrc  bnd)


