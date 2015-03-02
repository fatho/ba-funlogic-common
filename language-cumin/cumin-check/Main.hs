{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Monad.Writer
import           Data.Default.Class
import qualified Data.Map                     as M
import           FunLogic.Core.TypeChecker
import           Language.CuMin.ModBuilder
import           Language.CuMin.Prelude
import           Language.CuMin.TypeChecker
import qualified System.Environment           as Env
import qualified Text.PrettyPrint.ANSI.Leijen as PP

main :: IO ()
main = do
  (_, doc) <- runWriterT $ lift Env.getArgs >>= mapM_ checkFile
  PP.putDoc doc

checkFile :: FilePath -> WriterT PP.Doc IO ()
checkFile cuminFile = do
  tell $ PP.dullyellow (PP.text "Checking " <> PP.text cuminFile) <> PP.text "..." <> PP.line
  buildModuleFromFile cuminFile >>= \case
    Left msg -> tell msg
    Right modul ->
      case importUnqualified modul preludeModule of
        Left (adtConflicts, functionConflicts) ->
          let conflictNames = M.keys adtConflicts ++ M.keys functionConflicts
          in tell $ PP.text "Some names in the module conflict with prelude names:" PP.<$>
            PP.vsep (map PP.text conflictNames) PP.<> PP.line
        Right modulWithPrelude -> case evalTC (checkModule modulWithPrelude) def def of
          Left msg -> tell $ PP.pretty msg <> PP.line
          Right () -> tell $ PP.dullgreen $ PP.text "Success!" <> PP.line
