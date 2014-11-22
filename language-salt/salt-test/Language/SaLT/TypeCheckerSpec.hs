{-# LANGUAGE LambdaCase #-}
module Language.SaLT.TypeCheckerSpec where

import           Control.Monad
import           Data.Default.Class

import           FunLogic.Core.ModBuilder
import           FunLogic.Core.Pretty
import           FunLogic.Core.TypeChecker

import           Language.SaLT.AST
import           Language.SaLT.Parser
import           Language.SaLT.TypeChecker
import           Text.PrettyPrint.ANSI.Leijen (pretty)
import           Text.Trifecta.Result

import           Test.Hspec
import           Text.Printf

shouldHaveType :: Exp -> Type -> Expectation
shouldHaveType e ty = case evalTC (includeBuiltIns >> checkExp e) def def of
  Left msg -> expectationFailure $ show $ pretty msg
  Right ty' -> unless (ty' == ty) $
    expectationFailure $ printf "Expected type `%s`, but got `%s`" (show $ prettyType ty) (show $ prettyType ty')

shouldNotPassTC :: Exp -> Expectation
shouldNotPassTC e = shouldFail (evalTC (includeBuiltIns >> checkExp e) def def)

shouldFail :: Show a => Either e a -> Expectation
shouldFail (Left _) = return ()
shouldFail (Right a) = expectationFailure $ printf "Should have failed, but returned " (show a)

spec :: Spec
spec = do
  describe "accepts valid test files" $ do
    -- TODO: automatically determine files in "salt/valid" folder
    let files = ["salt/valid/Test.salt", "salt/valid/ADT.salt"]
    forM_ files $ \file -> it file $
      parseSaltFileEx file >>= \case
        Failure doc -> expectationFailure $ printf "could not parse file '%s': %s" file (show doc)
        Success decls -> case buildModule "Main" [adt | DData adt <- decls] [bnd | DTop bnd <- decls] of
          Left doc -> expectationFailure $ printf "could not build module from file '%s': %s" file (show doc)
          Right modul -> case evalTC (includeBuiltIns >> checkModule modul) def def of
            Left msg -> expectationFailure $ printf "type checker failed on '%s': %s" file (show $ pretty msg)
            Right () -> return ()

  describe "correctly determines type valid expressions" $ do
    it "addition" $
      EPrim PrimAdd [ELit (LInt 0), ELit (LInt 1)] `shouldHaveType` TNat
    it "lambda" $
      ELam "x" TNat (EVar "x") `shouldHaveType` TFun TNat TNat
    -- TODO: add more valid expressions

  describe "rejects incorrectly typed expressions" $
    it "addition of Nat and Bool" $
      shouldNotPassTC $ EPrim PrimAdd [ECon "False" [], ELit (LInt 0)]
    -- TODO: add more invalid expressions

  it "rejects invalid test files" $
    pendingWith "write invalid-syntax test files"
