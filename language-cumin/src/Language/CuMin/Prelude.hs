{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.CuMin.Prelude where

import           FunLogic.Core.ModBuilder
import           Language.CuMin.AST
import           Language.CuMin.TH

preludeDecls :: [Decl]
preludeDecls = $parseCuMinPrelude

preludeADTs :: [ADT]
preludeADTs = [adt | DData adt <- preludeDecls]

preludeBindings :: [Binding]
preludeBindings = [bnd | DTop bnd <- preludeDecls]

preludeModule :: Module
preludeModule = case buildModule "Prelude" preludeADTs preludeBindings of
  Left _ -> error "could not build CuMin prelude"
  Right prelude -> prelude
