{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
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
import           Data.Data

import           FunLogic.Core.AST

type Module = CoreModule Binding

-- | Top-level binding in CuMin.
data Binding = Binding
  { _bindingName :: BindingName
  -- ^ Name the expression is bound to.
  , _bindingArgs :: [VarName]
  -- ^ List of function arguments.
  , _bindingExpr :: Exp
  -- ^ Expression bound.
  , _bindingType :: TyDecl
  -- ^ Type declaration of this binding.
  , _bindingSrc  :: SrcRef
  -- ^ Source code location of the binding.
  } deriving (Show, Data, Typeable)

-- | A top level CuMin declaration.
data Decl
  = DTop Binding
  -- ^ Function declaration
  | DData ADT
  -- ^ Data declaration.
  deriving (Show, Data, Typeable)

-- | Data type of CuMin expression.
data Exp
  = EVar VarName
  -- ^ Referencing a local variable.
  | ELet VarName Exp Exp
  -- ^ Let binding of a variable. The first expression is bound to the name in the second expression.
  | ELetFree VarName Type Exp
  -- ^ Let binding of a free variable of the given type in the expression.
  | EFailed Type
  -- ^ Failure (bottom) of a given type.
  | EFun BindingName [Type]
  -- ^ Referenc to function with instantiation of polymorphic type variables.
  | EApp Exp Exp
  -- ^ Function application.
  | ELit Lit
  -- ^ Literal.
  | EPrim PrimOp [Exp]
  -- ^ Primitive operation.
  | ECon VarName [Type]
  -- ^ Reference constructor with instantiation of type arguments.
  | ECase Exp [Alt]
  -- ^ Case expression.
  deriving (Show, Data, Typeable)

-- | Primitive operations in CuMin.
data PrimOp
  = PrimAdd
  -- ^ Natural number addition.
  | PrimEq
  -- ^ Natural number equality.
  deriving (Show, Data, Typeable)

-- | Case alternative.
data Alt
  = Alt Pat Exp
  -- ^ An alternative consists of a pattern and a body expression.
  deriving (Show, Data, Typeable)

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


