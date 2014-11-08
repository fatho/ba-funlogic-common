{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
module FunLogic.Core.AST where

import           Control.Lens
import           Data.Data
import qualified Data.Map     as M

type Column = Int
type Row    = Int

data SrcRef
  = SrcRef
    { _srcFile  :: FilePath
    , _srcBegin :: (Row, Column)
    , _srcEnd   :: (Row, Column)
    } deriving (Eq, Ord, Data, Typeable)

instance Show SrcRef where
  show (SrcRef path (r1,c1) (r2,c2)) =
    path ++ ":(" ++ show r1 ++ ":" ++ show c1 ++ " - " ++ show r2 ++ ":" ++ show c2 ++ ")"

type Name = String
type TVName = String

data ADT = ADT
  { _adtName   :: Name
  , _adtTyArgs :: [TVName]
  , _adtConstr :: [ConDecl]
  , _adtSrcRef :: SrcRef
  } deriving (Show, Data, Typeable)

data ConDecl
  = ConDecl Name [Type]
  deriving (Show, Data, Typeable)

-- | Example: `forall a.Data a => (a,a)`
--   --> `TyDecl [RName "a"] [TyConstraint "Data" (RName "a")] (TCon "Pair" [(RName "a"), (RName "a")])`
data TyDecl
  = TyDecl [TVName] [TyConstraint] Type
  deriving (Show, Data, Typeable)

-- | Example: `Data a` --> `TyConstraint "Data" "a"`
data TyConstraint
  = TyConstraint Name TVName
  deriving (Show, Eq, Data, Typeable)

data Type
  = TVar  TVName
  | TCon  Name [Type]
  deriving (Show, Eq, Data, Typeable)

-- plated lenses
instance Plated Type

-- Lenses
makeLenses ''ADT

-- * Useful Type Pattern Synonyms
pattern TFun x y = TCon "->" [x, y]
pattern TNat     = TCon "Nat" []
pattern TTup x y = TCon "Pair" [x, y]

-- * Predefined ADTs

adtDefBool :: ADT
adtDefBool = ADT "Bool" []
  [ ConDecl "False" []
  , ConDecl "True" []
  ] srcBuiltIn
adtDefList :: ADT
adtDefList = ADT "List" ["a"]
  [ ConDecl "Nil" []
  , ConDecl "Cons" [TVar "a", TCon "List" [TVar "a"]]
  ] srcBuiltIn
adtDefPair :: ADT
adtDefPair = ADT "Pair" ["a", "b"]
  [ ConDecl "Pair" [TVar "a", TVar "b"]
  ] srcBuiltIn

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

