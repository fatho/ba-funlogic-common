{-# LANGUAGE PatternSynonyms, TemplateHaskell #-}
module FunLogic.Core.AST where

import Control.Lens

type Column = Int
type Row    = Int

data SrcRef
  = SrcRef
    { _srcFile  :: FilePath
    , _srcBegin :: (Row, Column)
    , _srcEnd   :: (Row, Column)
    } deriving (Show, Eq, Ord)

type Name = String
type TVName = String

data ADT = ADT
  { _adtName   :: Name
  , _adtTyArgs :: [TVName]
  , _adtConstr :: [ConDecl]
  , _adtSrcRef :: SrcRef
  } deriving (Show)

data ConDecl
  = ConDecl Name [Type]
  deriving (Show)

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
pattern TTup x y = TCon "Pair" [x, y]

-- Lenses
makeLenses ''ADT