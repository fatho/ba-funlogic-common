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
test = [salt|
test :: a -> a
test = \x :: a -> x
|]
