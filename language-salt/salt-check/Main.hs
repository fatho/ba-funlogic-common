{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Monad.Writer
import           Data.Default.Class
import           FunLogic.Core.TypeChecker
import           Language.SaLT.AST
import           Language.SaLT.ModBuilder
import           Language.SaLT.Prelude
import           Language.SaLT.TypeChecker

import qualified Text.PrettyPrint.ANSI.Leijen as PP

import           System.Environment           (getArgs)

main :: IO ()
main = do
  putStrLn "Usage: salt-check [--cumin-prelude] FILES ..."
  args <- getArgs
  let (files, prelude) =
        case args of
          "--cumin-prelude":files -> (files, cuminPreludeModule)
          _ -> (args, preludeModule)
  (_, doc) <- runWriterT $ mapM_ (checkFile prelude) files
  PP.putDoc doc

checkFile :: Module -> FilePath -> WriterT PP.Doc IO ()
checkFile prelude saltFile = do
  tell $ PP.dullyellow (PP.text "Checking " <> PP.text saltFile) <> PP.text "..." <> PP.line
  buildModuleFromFile saltFile >>= \case
    Left msg    -> tell msg
    Right modul -> case evalTC (unsafeIncludeModule prelude >> checkModule modul) def def of
      Left msg -> tell $ PP.pretty msg <> PP.line
      Right () -> tell $ PP.dullgreen $ PP.text "Success!" <> PP.line
