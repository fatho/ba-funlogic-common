module Language.SaLT.ParserDef where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import qualified Data.HashSet as HS
import           Safe
import qualified Text.Parser.Token.Highlight as H
import           Text.Trifecta
import           Text.Trifecta.Combinators
import           Text.Trifecta.Delta
import           Text.Trifecta.Indentation

import           Language.SaLT.AST


type SaltParser = IndentationParserT Char Parser

parseSaltTest :: (MonadIO m, Show a) => SaltParser a -> String -> m ()
parseSaltTest p xs = do
  liftIO $ putStrLn xs
  liftIO $ putStrLn "-----------------------------------"
  parseTest (runSaltParser p) xs

runSaltParser :: SaltParser a -> Parser a
runSaltParser p = evalIndentationParserT p $ mkIndentationState 0 infIndentation False Gt

saltVarStyle :: IdentifierStyle SaltParser
saltVarStyle = IdentifierStyle
            { _styleName      = "identifier"
            , _styleStart     = lower
            , _styleLetter    = alphaNum <|> oneOf "_'"
            , _styleReserved = HS.fromList
                [ "failed", "unknown"
                , "forall", "case", "of"
                ]
            , _styleHighlight = H.Identifier
            , _styleReservedHighlight = H.ReservedIdentifier
            }

saltConStyle :: IdentifierStyle SaltParser
saltConStyle = IdentifierStyle
            { _styleName      = "constructor"
            , _styleStart     = upper
            , _styleLetter    = alphaNum <|> oneOf "_'"
            , _styleReserved  = HS.fromList ["Set"]
            , _styleHighlight = H.Constructor
            , _styleReservedHighlight = H.ReservedConstructor
            }

conIdent :: SaltParser Name 
conIdent   = ident saltConStyle

reservedCon :: String -> SaltParser ()
reservedCon = reserve saltConStyle

varIdent :: SaltParser Name 
varIdent   = ident saltVarStyle

reserved :: String -> SaltParser ()
reserved = reserve saltVarStyle
