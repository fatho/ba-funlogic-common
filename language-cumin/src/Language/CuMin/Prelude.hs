{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.CuMin.Prelude where

import           FunLogic.Core.ModBuilder
import           Language.CuMin.AST
import           Language.CuMin.TH
import           Language.CuMin.ModBuilder

preludeModule :: Module
preludeModule = $(moduleFromFile "cumin/Prelude.cumin")