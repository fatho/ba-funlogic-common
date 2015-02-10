{-# LANGUAGE LambdaCase #-}
module Language.SaLT.ParserSpec where

import           Test.Hspec

spec :: Spec
spec = it "test specific edge-cases of parser" $
  pendingWith "whole-file parsing is part of the type-checker test-suite"
