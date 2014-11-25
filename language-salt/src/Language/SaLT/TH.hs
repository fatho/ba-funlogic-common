module Language.SaLT.TH where

import           Control.Monad.State       (liftIO)
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Text.Trifecta.Result

import           Language.SaLT.Parser

parseSaltFileQ :: String -> FilePath -> Q Exp
parseSaltFileQ name file = do
  content <- runIO $ liftIO $ readFile file
  parseSaltExQ name content

parseSaltExQ :: String -> String -> Q Exp
parseSaltExQ name content =
  case parseSaltString name content of
    Success decls -> dataToExpQ (const Nothing) decls
    Failure msg -> fail $ "Parsing quasi quote failed:\n`" ++ show msg ++ "`\nQuasi-quoted code:\n`" ++ content ++ "`\n"

parseSaltQ :: String -> Q Exp
parseSaltQ = parseSaltExQ "<quasi-quoted>"

parseSaltPrelude :: Q Exp
parseSaltPrelude = parseSaltFileQ "<prelude>" "salt/Prelude.salt"

salt :: QuasiQuoter
salt = QuasiQuoter parseSaltQ undefined undefined undefined
