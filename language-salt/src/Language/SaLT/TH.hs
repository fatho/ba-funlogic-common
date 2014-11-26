module Language.SaLT.TH where

import           Control.Applicative
import           Control.Monad
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Text.Trifecta
import           Text.Trifecta.Indentation

import           FunLogic.Core.TH
import           Language.SaLT.Parser

parseSaltFileDeclsQ :: String -> FilePath -> Q Exp
parseSaltFileDeclsQ name = runParserOnFileQ program name >=> dataToExp

parseSaltPrelude :: Q Exp
parseSaltPrelude = runParserOnFileQ program "<prelude>" "salt/Prelude.salt" >>= dataToExp

saltDecls :: QuasiQuoter
saltDecls = parserToQQ program

saltExp :: QuasiQuoter
saltExp = parserToQQ (whiteSpace *> expression <* whiteSpace <* eof)

saltPat :: QuasiQuoter
saltPat = parserToQQ (whiteSpace *> patternP <* whiteSpace <* eof)

saltBinding :: QuasiQuoter
saltBinding = parserToQQ . absoluteIndentation $ whiteSpace *> binding <* eof
