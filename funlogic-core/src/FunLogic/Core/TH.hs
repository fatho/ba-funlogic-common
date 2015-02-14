{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
module FunLogic.Core.TH where

import           Control.Applicative
import           Control.Monad
import           Data.Data
import qualified Data.Map                     as Map
import qualified Language.Haskell.TH          as TH
import qualified Language.Haskell.TH.Quote    as TH
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           Text.Trifecta
import           Text.Trifecta.Indentation

import qualified FunLogic.Core.AST            as AST
import qualified FunLogic.Core.Parser         as Parser

-- | Runs a parser in the template haskell monad.
runParserQ :: (Parser.RunnableParsing m, Data a) => m a -> String -> String -> TH.Q a
runParserQ parser name content =
  checkFailure $ Parser.runParser name content parser

-- | Parses a file in the template haskell monad.
runParserOnFileQ :: (Parser.RunnableParsing m, Data a) => m a -> String -> String -> TH.Q a
runParserOnFileQ parser name fileName = do
  content <- TH.runIO $ readFile fileName
  checkFailure $ Parser.runParser name content parser

-- | Workaround to handle 'Map.Map' in 'TH.dataToExpQ'. Map's Data instance reports a wrong constructor name, so
-- 'TH.dataToExpQ' cannot transform it to a template haskell instruction.
qqMapADT :: Map.Map String AST.ADT -> Maybe TH.ExpQ
qqMapADT m = let list = TH.dataToExpQ (const Nothing) (Map.toAscList m)
  in Just [|Map.fromAscList $list|]

-- | Returns the value on success, fails with the error message otherwise.
checkFailure :: (Data a) => Result a -> TH.Q a
checkFailure = \case
  Success a -> return a
  Failure msg -> fail $ "Parsing quasi quote failed:\n`" ++ show (PP.plain msg) ++ "`\n"

-- | Constructs an expression quasi-quoter from a parsing function and a functions transforming the value to a TH expression.
makeQQ :: Data a => (a -> TH.ExpQ) -> (String -> TH.Q a) -> TH.QuasiQuoter
makeQQ toQ f = TH.QuasiQuoter (f >=> toQ) undefined undefined undefined

-- | Transforms a parser to a quasi-quoter using 'makeQQ'.
parserToQQ :: (Parser.RunnableParsing m, Data a) => (a -> TH.ExpQ) -> m a -> TH.QuasiQuoter
parserToQQ toQ p = makeQQ toQ $ runParserQ p "<quasi-quote>"

-- | A quasi quoter for 'AST.Type' values.
ty :: TH.QuasiQuoter
ty = parserToQQ (TH.dataToExpQ (const Nothing))
  (whiteSpace *> Parser.functionType <* eof :: Parser.IndentParser AST.Type)

-- | A quasi quoter for 'AST.ADT' values.
adt :: TH.QuasiQuoter
adt = parserToQQ (TH.dataToExpQ (const Nothing))
  (absoluteIndentation (whiteSpace *> Parser.adtParser <* eof :: Parser.IndentParser AST.ADT))
