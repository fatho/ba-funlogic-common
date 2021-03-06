{-# LANGUAGE LambdaCase #-}
module Language.CuMin.TypeCheckerSpec where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Default.Class
import qualified System.Directory             as Dir
import qualified System.FilePath              as File
import           Test.Hspec
import           Text.PrettyPrint.ANSI.Leijen (pretty)
import           Text.Printf
import           Text.Trifecta.Result

import           FunLogic.Core.Pretty
import           FunLogic.Core.TypeChecker
import           Language.CuMin.AST
import           Language.CuMin.ModBuilder
import           Language.CuMin.Parser
import           Language.CuMin.Prelude
import           Language.CuMin.TypeChecker

-- | Returns true when the file name's extension matches the first argument.
fileExtensionIs :: String -> FilePath -> Bool
fileExtensionIs ext path = File.takeExtension path == ext

-- | Returns all files in a given directory matching the predicate.
getDirectoryFiles :: MonadIO m => (FilePath -> m Bool) -> FilePath -> m [FilePath]
getDirectoryFiles p dir = map (dir File.</>) `liftM` liftIO (Dir.getDirectoryContents dir)
  >>= filterM (liftIO . Dir.doesFileExist)
  >>= filterM p

-- | Extends scope with built-ins and prelude module.
prepareTC :: TC CuMinErrCtx ()
prepareTC = includeBuiltIns >> unsafeIncludeModule preludeModule

-- | States the expectation that the given expression has a specific type when type-checked with the default-prelude.
shouldHaveType :: Exp -> Type -> Expectation
shouldHaveType e ty = case evalTC (prepareTC >> checkExp e) def def of
  Left msg -> expectationFailure $ show $ pretty msg
  Right ty' -> unless (ty' == ty) $
    expectationFailure $ printf "Expected type `%s`, but got `%s`" (show $ prettyType ty) (show $ prettyType ty')

-- | States the expectation, that the given expression has no valid type.
shouldNotPassTC :: Exp -> Expectation
shouldNotPassTC e = shouldFail (evalTC (prepareTC >> checkExp e) def def)

-- | States the expectation, that a result should be an error message.
shouldFail :: Show a => Either e a -> Expectation
shouldFail (Left _) = return ()
shouldFail (Right a) = expectationFailure $ printf "Should have failed, but returned " (show a)

-- | Type-checker spec.
spec :: Spec
spec = do
  describe "accepts valid test files" $ do
    files <- runIO $ getDirectoryFiles (return . fileExtensionIs ".cumin") "cumin/valid"
    forM_ files $ \file -> it file $
      parseCuMinFileEx file >>= \case
        Failure doc -> expectationFailure $ printf "could not parse file '%s': %s" file (show doc)
        Success decls -> case buildModuleFromDecls "Main" decls of
          Left doc -> expectationFailure $ printf "could not build module from file '%s': %s" file (show doc)
          Right modul -> case evalTC (prepareTC >> checkModule modul) def def of
            Left msg -> expectationFailure $ printf "type checker failed on '%s': %s" file (show $ pretty msg)
            Right () -> return ()

  describe "correctly determines type valid expressions" $ do
    it "addition" $
      EPrim PrimAdd [ELit (LNat 0), ELit (LNat 1)] `shouldHaveType` TNat
    it "other" $
      pendingWith "add more valid expressions"
    -- TODO: add more valid expressions

  describe "rejects incorrectly typed expressions" $ do
    it "addition of Nat and Bool" $
      shouldNotPassTC $ EPrim PrimAdd [ECon "False" [], ELit (LNat 0)]
    it "other" $
      pendingWith "add more invalid expressions"
    -- TODO: add more invalid expressions

  it "rejects invalid test files" $
    pendingWith "write invalid-syntax test files"
