{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Monad.Writer
import           Data.Default.Class
import           FunLogic.Core.TypeChecker
import           Language.CuMin.ModBuilder
import           Language.CuMin.TypeChecker
import           Language.CuMin.Prelude

import           Text.PrettyPrint.ANSI.Leijen hiding ((<>))

import           System.Environment           (getArgs)

main :: IO ()
main = do
  (_, doc) <- runWriterT $ lift getArgs >>= mapM_ checkFile
  putDoc doc

checkFile :: FilePath -> WriterT Doc IO ()
checkFile cuminFile = do
  tell $ dullyellow (text "Checking " <> text cuminFile) <> text "..." <> line
  buildModuleFromFile cuminFile >>= \case
    Left msg    -> tell msg
    Right modul -> case evalTC (unsafeIncludeModule preludeModule >> checkModule modul) def def of
      Left msg -> tell $ prettyErr msg <> line
      Right () -> tell $ dullgreen $ text "Success!" <> line
