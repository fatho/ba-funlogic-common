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
