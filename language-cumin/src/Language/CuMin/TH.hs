module Language.CuMin.TH where

import           Control.Monad.State       (liftIO)
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Text.Trifecta.Result

import           Language.CuMin.Parser

parseCuMinFileQ :: String -> FilePath -> Q Exp
parseCuMinFileQ name file = do
  content <- runIO $ liftIO $ readFile file
  parseCuMinExQ name content

parseCuMinExQ :: String -> String -> Q Exp
parseCuMinExQ name content =
  case parseCuMinString name content of
    Success decls -> dataToExpQ (const Nothing) decls
    Failure msg -> fail $ "Parsing quasi quote failed:\n`" ++ show msg ++ "`\nQuasi-quoted code:\n`" ++ content ++ "`\n"

parseCuMinQ :: String -> Q Exp
parseCuMinQ = parseCuMinExQ "<quasi-quoted>"

parseCuMinPrelude :: Q Exp
parseCuMinPrelude = parseCuMinFileQ "<prelude>" "cumin/Prelude.cumin"

cumin :: QuasiQuoter
cumin = QuasiQuoter parseCuMinQ undefined undefined undefined
