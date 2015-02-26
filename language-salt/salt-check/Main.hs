{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Monad.Writer
import           Data.Default.Class
import           FunLogic.Core.TypeChecker
import           Language.SaLT.ModBuilder
import           Language.SaLT.Prelude
import           Language.SaLT.TypeChecker

import qualified Text.PrettyPrint.ANSI.Leijen as PP

import           System.Environment           (getArgs)

main :: IO ()
main = do
  (_, doc) <- runWriterT $ lift getArgs >>= mapM_ checkFile
  PP.putDoc doc

checkFile :: FilePath -> WriterT PP.Doc IO ()
checkFile saltFile = do
  tell $ PP.dullyellow (PP.text "Checking " <> PP.text saltFile) <> PP.text "..." <> PP.line
  buildModuleFromFile saltFile >>= \case
    Left msg    -> tell msg
    Right modul -> case evalTC (unsafeIncludeModule preludeModule >> checkModule modul) def def of
      Left msg -> tell $ PP.pretty msg <> PP.line
      Right () -> tell $ PP.dullgreen $ PP.text "Success!" <> PP.line
