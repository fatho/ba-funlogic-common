{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Monad.Writer
import           Data.Default.Class
import           FunLogic.Core.TypeChecker
import           Language.SaLT.AST
import           Language.SaLT.ModBuilder
import           Language.SaLT.Parser
import           Language.SaLT.TypeChecker

import           Text.PrettyPrint.ANSI.Leijen hiding ((<>))
import           Text.Trifecta.Result

import           System.Environment           (getArgs)

main :: IO ()
main = do
  (_, doc) <- runWriterT $ lift getArgs >>= mapM_ checkFile
  putDoc doc

checkFile :: FilePath -> WriterT Doc IO ()
checkFile saltFile = do
  tell $ dullyellow (text "Checking " <> text saltFile) <> text "..." <> line
  parseSaltFileEx saltFile >>= \case
    Failure msg -> tell $ msg <> line
    Success decls -> case buildModule decls of
      Left msg    -> tell $ text msg
      Right modul -> case evalTC (includeBuiltIns >> checkModule modul) def def of
        Left msg -> tell $ prettyErr msg <> line
        Right () -> tell $ dullgreen $ text "Success!" <> line
