{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module FunLogic.Core.TH where

import           Control.Applicative
import           Control.Monad
import           Data.Data
import           Data.Map
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Text.Trifecta
import           Text.Trifecta.Indentation

import qualified FunLogic.Core.AST         as AST
import           FunLogic.Core.Parser

runParserQ :: (RunnableParsing m, Data a) => m a -> String -> String -> Q a
runParserQ parser name content =
  checkFailure $ runParser name content parser

runParserOnFileQ :: (RunnableParsing m, Data a) => m a -> String -> String -> Q a
runParserOnFileQ parser name fileName = do
  content <- runIO $ readFile fileName
  checkFailure $ runParser name content parser

-- Workaround: `Map`s are not handled correctly by dataToExpQ, so must be
-- handled manually:
qqMapADT :: Map String AST.ADT -> Maybe (Q Exp)
qqMapADT m = let list = dataToExpQ (const Nothing) (toAscList m)
  in Just [|fromAscList $list|]

checkFailure :: (Data a) => Result a -> Q a
checkFailure = \case
  Success a -> return a
  Failure msg -> fail $ "Parsing quasi quote failed:\n`" ++ show msg ++ "`\n"

makeQQ :: Data a => (a -> Q Exp) -> (String -> Q a) -> QuasiQuoter
makeQQ toQ f = QuasiQuoter (f >=> toQ) undefined undefined undefined

parserToQQ :: (RunnableParsing m, Data a) => (a -> Q Exp) -> m a -> QuasiQuoter
parserToQQ toQ p = makeQQ toQ $ runParserQ p "<quasi-quote>"

ty :: QuasiQuoter
ty = parserToQQ (dataToExpQ (const Nothing))
  (whiteSpace *> functionType <* eof :: IndentParser AST.Type)

adt :: QuasiQuoter
adt = parserToQQ (dataToExpQ (const Nothing))
  (absoluteIndentation (whiteSpace *> adtParser <* eof :: IndentParser AST.ADT))
