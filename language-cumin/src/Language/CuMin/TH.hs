{-# LANGUAGE TemplateHaskell #-}
module Language.CuMin.TH
  ( cuminDecls
  , cuminExp
  , cuminPat
  , cuminBinding
  , parseCuMinPrelude
  , parseCuMinFileDeclsQ
  , dataToExp
  , module FunLogic.Core.TH
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Data
import           Data.Generics
import           Data.Map
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Text.Trifecta
import           Text.Trifecta.Indentation

import           FunLogic.Core.TH
import qualified Language.CuMin.AST as AST
import           Language.CuMin.Parser

-- * Workaround

-- What follows is a workaround that is necessary
-- because dataToExpQ does not handle `Map`s correctly.
qqMapBnd :: Map String AST.Binding -> Maybe (Q Exp)
qqMapBnd m = let list = dataToExpQ (const Nothing) (toAscList m)
  in Just [|fromAscList $list|]

qqAll :: Data a => a -> Maybe (Q Exp)
qqAll = const Nothing `extQ` qqMapADT `extQ` qqMapBnd

dataToExp :: Data a => a -> Q Exp
dataToExp = dataToExpQ qqAll

-- * QuasiQuoters

parseCuMinFileDeclsQ :: String -> FilePath -> Q Exp
parseCuMinFileDeclsQ name = runParserOnFileQ program name >=> dataToExp

parseCuMinPrelude :: Q Exp
parseCuMinPrelude = runParserOnFileQ program "<prelude>" "cumin/Prelude.cumin" >>= dataToExp

cuminDecls :: QuasiQuoter
cuminDecls = parserToQQ dataToExp program

cuminExp :: QuasiQuoter
cuminExp = parserToQQ dataToExp (whiteSpace *> expression <* whiteSpace <* eof)

cuminPat :: QuasiQuoter
cuminPat = parserToQQ dataToExp (whiteSpace *> patternP <* whiteSpace <* eof)

cuminBinding :: QuasiQuoter
cuminBinding = parserToQQ dataToExp . absoluteIndentation $ whiteSpace *> binding <* eof
