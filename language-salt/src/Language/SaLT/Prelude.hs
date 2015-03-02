{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.SaLT.Prelude where

import           Language.SaLT.AST
import           Language.SaLT.TH

-- | A SaLT module containing prelude definitions.
preludeModule :: Module
preludeModule = $(moduleFromFile "salt/Prelude.salt")

-- | A SaLT module containing the translated CuMin prelude definitions.
cuminPreludeModule :: Module
cuminPreludeModule = $(moduleFromFile "salt/CuMinPrelude.salt")
