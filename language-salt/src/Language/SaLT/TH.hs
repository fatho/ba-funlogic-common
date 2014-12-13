{-# LANGUAGE TemplateHaskell #-}
module Language.SaLT.TH
  ( saltDecls
  , saltExp
  , saltPat
  , saltBinding
  , parseSaltPrelude
  , parseSaltFileDeclsQ
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
import qualified Language.SaLT.AST as AST
import           Language.SaLT.Parser

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

parseSaltFileDeclsQ :: String -> FilePath -> Q Exp
parseSaltFileDeclsQ name = runParserOnFileQ program name >=> dataToExp

parseSaltPrelude :: Q Exp
parseSaltPrelude = runParserOnFileQ program "<prelude>" "salt/Prelude.salt" >>= dataToExp

saltDecls :: QuasiQuoter
saltDecls = parserToQQ dataToExp program

saltExp :: QuasiQuoter
saltExp = parserToQQ dataToExp (whiteSpace *> expression <* whiteSpace <* eof)

saltPat :: QuasiQuoter
saltPat = parserToQQ dataToExp (whiteSpace *> patternP <* whiteSpace <* eof)

saltBinding :: QuasiQuoter
saltBinding = parserToQQ dataToExp . absoluteIndentation $ whiteSpace *> binding <* eof
