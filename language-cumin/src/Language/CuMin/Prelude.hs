{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.CuMin.Prelude where

import           Language.CuMin.AST
import           Language.CuMin.TH

-- | A CuMin module containing prelude definitions.
preludeModule :: Module
preludeModule = $(moduleFromFile "cumin/Prelude.cumin")