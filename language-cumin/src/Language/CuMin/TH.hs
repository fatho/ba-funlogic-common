module Language.CuMin.TH where

import           Control.Applicative
import           Control.Monad
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Text.Trifecta
import           Text.Trifecta.Indentation

import           FunLogic.Core.TH
import           Language.CuMin.Parser

parseCuMinFileDeclsQ :: String -> FilePath -> Q Exp
parseCuMinFileDeclsQ name = runParserOnFileQ program name >=> dataToExp

parseCuMinPrelude :: Q Exp
parseCuMinPrelude = runParserOnFileQ program "<prelude>" "cumin/Prelude.cumin" >>= dataToExp

cuminDecls :: QuasiQuoter
cuminDecls = parserToQQ program

cuminExp :: QuasiQuoter
cuminExp = parserToQQ (whiteSpace *> expression <* whiteSpace <* eof)

cuminPat :: QuasiQuoter
cuminPat = parserToQQ (whiteSpace *> patternP <* whiteSpace <* eof)

cuminBinding :: QuasiQuoter
cuminBinding = parserToQQ . absoluteIndentation $ whiteSpace *> binding <* eof
