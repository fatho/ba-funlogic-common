{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies    #-}
module Language.SaLT.AST
  ( Module
  , Binding(..)
  , Decl(..)
  , module FunLogic.Core.AST
  , Exp(..)
  , Lit(..)
  , PrimOp(..)
  , Alt(..)
  , Pat(..)
  ) where

import           Control.Applicative
import           Data.Typeable
import           Data.Data

import           FunLogic.Core.AST

type Module = CoreModule Binding

data Binding = Binding
  { _bindingName :: Name
  , _bindingExpr :: Exp
  , _bindingType :: TyDecl
  , _bindingSrc  :: SrcRef
  } deriving (Show, Typeable, Data)

data Decl
  = DTop Binding
  | DData ADT
  deriving (Show, Typeable, Data)

data Exp
  = EVar Name
  | EFun Name [Type]
  | ELam Name Type Exp
  | EApp Exp Exp
  | ELit Lit
  | EPrim PrimOp [Exp]
  | ECon Name [Type]
  | ESet Exp
  | ECase Exp [Alt]
  | EFailed Type
  | EUnknown Type
  deriving (Show, Typeable, Data)

instance HasPrecedence Exp where
  prec = \case
    ELam _ _ _ -> 1
    ECase _ _ -> 1

    -- As specified, '+' binds most and '>>=' least tightly.
    EPrim p _ -> case p of
      PrimBind -> 2
      PrimEq -> 3
      PrimAdd -> 4

    -- Application binds more tightly than primitives.
    EApp _ _ -> 5

    -- In the following cases, parentheses are never needed:
    EVar _ -> 6
    EFailed _ -> 6
    EUnknown _ -> 6
    ELit _ -> 6
    EFun _ _ -> 6
    ECon _ _ -> 6
    ESet _ -> 6

data PrimOp
  = PrimAdd
  | PrimEq
  | PrimBind
  deriving (Show, Data, Typeable)

data Alt
  = Alt Pat Exp
  deriving (Show, Typeable, Data)

-- Lenses

instance IsBinding Binding where
  type BindingExp Binding = Exp
  bindingName f bnd = (\x -> bnd {_bindingName = x}) <$> f (_bindingName bnd)
  bindingExpr f bnd = (\x -> bnd {_bindingExpr = x}) <$> f (_bindingExpr bnd)
  bindingType f bnd = (\x -> bnd {_bindingType = x}) <$> f (_bindingType bnd)
  bindingSrc  f bnd = (\x -> bnd {_bindingSrc  = x}) <$> f (_bindingSrc  bnd)
