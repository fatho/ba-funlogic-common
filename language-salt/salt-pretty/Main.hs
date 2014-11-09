{-# LANGUAGE LambdaCase #-}
module Main where

import           System.Environment           (getArgs)
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import           Text.Trifecta.Result

import           FunLogic.Core.Pretty
import           Language.SaLT.ModBuilder
import           Language.SaLT.Parser
import           Language.SaLT.Pretty


main :: IO ()
main = do
  args <- getArgs
  case args of
    (w:f:fs) -> mapM_ (prettyPrintFile (read w)) $ f:fs
    _ -> putStrLn "Usage: salt-check <page-width> <file1>.salt <file2>.salt ..."
  where
  prettyPrintFile wid file = parseSaltFileEx file >>= \case
    Failure msg -> putDoc $ msg <> line
    Success decls -> case buildModule decls of
      Left msg  -> putDoc . red . text $ msg
      Right m -> do
        putDoc (green . text $ "Pretty printing `" ++ file ++ "`:" ++ "\n------\n")
        displayPretty wid (prettyModule m)
        putDoc (green . text $ "\n------\n")
