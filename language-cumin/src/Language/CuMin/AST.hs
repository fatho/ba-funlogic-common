{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE LambdaCase      #-}
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

data PrimOp
  = PrimAdd
  | PrimEq
  deriving (Show)

data Alt
  = Alt Pat Exp
  deriving (Show)

instance HasPrecedence Exp where
  prec = \case
    ELet _ _ _ -> 1
    ELetFree _ _ _ -> 1
    ECase _ _ -> 1

    -- As specified, '+' binds most and '>>=' least tightly.
    EPrim p _ -> case p of
      PrimEq -> 3
      PrimAdd -> 4

    -- Application binds more tightly than primitives.
    EApp _ _ -> 5

    -- In the following cases, parentheses are never needed:
    EVar _ -> 6
    EFailed _ -> 6
    ELit _ -> 6
    EFun _ _ -> 6
    ECon _ _ -> 6

-- Lenses
makeLensesFor [("_bindingArgs", "bindingArgs")] ''Binding

instance IsBinding Binding where
  type BindingExp Binding = Exp
  bindingName f bnd = (\x -> bnd {_bindingName = x}) <$> f (_bindingName bnd)
  bindingExpr f bnd = (\x -> bnd {_bindingExpr = x}) <$> f (_bindingExpr bnd)
  bindingType f bnd = (\x -> bnd {_bindingType = x}) <$> f (_bindingType bnd)
  bindingSrc  f bnd = (\x -> bnd {_bindingSrc  = x}) <$> f (_bindingSrc  bnd)


