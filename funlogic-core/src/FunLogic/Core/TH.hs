{-# LANGUAGE LambdaCase #-}
module FunLogic.Core.TH where

import           Control.Applicative
import           Control.Monad
import           Data.Data
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

dataToExp :: Data a => a -> Q Exp
dataToExp = dataToExpQ (const Nothing)

checkFailure :: (Data a) => Result a -> Q a
checkFailure = \case
  Success a -> return a
  Failure msg -> fail $ "Parsing quasi quote failed:\n`" ++ show msg ++ "`\n"

makeQQ :: Data a => (String -> Q a) -> QuasiQuoter
makeQQ f = QuasiQuoter (f >=> dataToExp) undefined undefined undefined

parserToQQ :: (RunnableParsing m, Data a) => m a -> QuasiQuoter
parserToQQ p = makeQQ $ runParserQ p "<quasi-quote>"

ty :: QuasiQuoter
ty = parserToQQ (whiteSpace *> functionType <* eof :: IndentParser AST.Type)

adt :: QuasiQuoter
adt = parserToQQ $ absoluteIndentation (whiteSpace *> adtParser <* eof :: IndentParser AST.ADT)
