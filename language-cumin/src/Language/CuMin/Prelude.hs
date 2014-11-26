{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.CuMin.Prelude where

import           Language.CuMin.AST
import           Language.CuMin.TH

preludeDecls :: [Decl]
preludeDecls = $parseCuMinPrelude

preludeADTs :: [ADT]
preludeADTs = [adt | DData adt <- preludeDecls]

preludeBindings :: [Binding]
preludeBindings = [bnd | DTop bnd <- preludeDecls]

test :: [Decl]
test = [cuminDecls|
test :: forall a. a -> a
test x = x

foo :: Bool -> Bool
foo b = case b of
  True -> True
  False -> False
|]
