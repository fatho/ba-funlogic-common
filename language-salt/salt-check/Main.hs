{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Monad.Writer
import           Data.Default.Class
import           FunLogic.Core.TypeChecker
import           Language.SaLT.ModBuilder
import           Language.SaLT.TypeChecker

import           Text.PrettyPrint.ANSI.Leijen hiding ((<>))

import           System.Environment           (getArgs)

main :: IO ()
main = do
  (_, doc) <- runWriterT $ lift getArgs >>= mapM_ checkFile
  putDoc doc

checkFile :: FilePath -> WriterT Doc IO ()
checkFile saltFile = do
  tell $ dullyellow (text "Checking " <> text saltFile) <> text "..." <> line
  buildModuleFromFile saltFile >>= \case
    Left msg    -> tell msg
    Right modul -> case evalTC (checkModule modul) def def of
      Left msg -> tell $ prettyErr msg <> line
      Right () -> tell $ dullgreen $ text "Success!" <> line
