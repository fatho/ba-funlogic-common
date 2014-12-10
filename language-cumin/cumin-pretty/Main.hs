{-# LANGUAGE LambdaCase #-}
module Main where

import           System.Environment           (getArgs)
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import           Text.Trifecta.Result

import           FunLogic.Core.Pretty
import           FunLogic.Core.ModBuilder
import           Language.CuMin.AST
import           Language.CuMin.Parser
import           Language.CuMin.Pretty


main :: IO ()
main = do
  args <- getArgs
  case args of
    (w:f:fs) -> mapM_ (prettyPrintFile (read w)) $ f:fs
    _ -> putStrLn "Usage: cumin-check <page-width> <file1>.cumin <file2>.cumin ..."
  where
  prettyPrintFile wid file = parseCuMinFileEx file >>= \case
    Failure msg -> putDoc $ msg <> line
    Success decls -> case buildModule "Main" [adt | DData adt <- decls] [bnd | DTop bnd <- decls] of
      Left msg  -> putDoc $ red msg
      Right m -> do
        putDoc (green . text $ "Pretty printing `" ++ file ++ "`:" ++ "\n------\n")
        displayPretty wid (prettyModule m)
        putDoc (green . text $ "\n------\n")
