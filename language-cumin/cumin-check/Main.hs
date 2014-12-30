{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Monad.Writer
import           Data.Default.Class
import           FunLogic.Core.TypeChecker
import           Language.CuMin.ModBuilder
import           Language.CuMin.TypeChecker
import           Language.CuMin.Prelude
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import qualified System.Environment          as Env

main :: IO ()
main = do
  (_, doc) <- runWriterT $ lift Env.getArgs >>= mapM_ checkFile
  PP.putDoc doc

checkFile :: FilePath -> WriterT PP.Doc IO ()
checkFile cuminFile = do
  tell $ PP.dullyellow (PP.text "Checking " <> PP.text cuminFile) <> PP.text "..." <> PP.line
  buildModuleFromFile cuminFile >>= \case
    Left msg    -> tell msg
    Right modul -> case evalTC (unsafeIncludeModule preludeModule >> checkModule modul) def def of
      Left msg -> tell $ PP.pretty msg <> PP.line
      Right () -> tell $ PP.dullgreen $ PP.text "Success!" <> PP.line
