{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.SaLT.Prelude where

import           FunLogic.Core.ModBuilder
import           Language.SaLT.AST
import           Language.SaLT.TH

preludeDecls :: [Decl]
preludeDecls = $parseSaltPrelude

preludeADTs :: [ADT]
preludeADTs = [adt | DData adt <- preludeDecls]

preludeBindings :: [Binding]
preludeBindings = [bnd | DTop bnd <- preludeDecls]

preludeModule :: Module
preludeModule = case buildModule "Prelude" preludeADTs preludeBindings of
  Left _ -> error "could not build SaLT prelude"
  Right prelude -> prelude