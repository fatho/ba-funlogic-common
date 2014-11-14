{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Monad.Writer
import           Data.Default.Class
import           FunLogic.Core.TypeChecker
import           FunLogic.Core.ModBuilder
import           Language.CuMin.AST
import           Language.CuMin.Parser
import           Language.CuMin.TypeChecker

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
  parseCuMinFileEx saltFile >>= \case
    Failure msg -> tell $ msg <> line
    Success decls -> case buildModule "Main" [adt | DData adt <- decls] [bnd | DTop bnd <- decls] of
      Left msg    -> tell msg
      Right modul -> case evalTC (includeBuiltIns >> checkModule modul) def def of
        Left msg -> tell $ prettyErr msg <> line
        Right () -> tell $ dullgreen $ text "Success!" <> line
