{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.SaLT.Prelude where

import           Language.SaLT.AST
import           Language.SaLT.TH

preludeDecls :: [Decl]
preludeDecls = $parseSaltPrelude

preludeADTs :: [ADT]
preludeADTs = [adt | DData adt <- preludeDecls]

preludeBindings :: [Binding]
preludeBindings = [bnd | DTop bnd <- preludeDecls]

test :: [Decl]
test = [saltDecls|
test :: forall a. a -> a
test = \x :: a -> x

foo :: Bool -> Bool
foo = \b :: Bool -> case b of
  True -> True
  False -> False
|]
