{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.SaLT.Prelude where

import           Language.SaLT.AST
import           Language.SaLT.TH

-- | A CuMin module containing prelude definitions.
preludeModule :: Module
preludeModule = $(moduleFromFile "salt/Prelude.salt")