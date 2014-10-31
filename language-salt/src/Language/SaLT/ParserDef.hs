{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, FlexibleContexts, TemplateHaskell #-}
module Language.SaLT.ParserDef where

import           Control.Applicative
import           Control.Arrow
import           Control.Lens hiding (noneOf)
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


newtype SaltParser a = SaltParser { runSaltParser' :: StateT SaltPState (IndentationParserT Char Parser) a }
  deriving ( Functor, Applicative, Alternative, Monad, MonadPlus, MonadState SaltPState)

deriving instance Parsing SaltParser
deriving instance CharParsing SaltParser
deriving instance DeltaParsing SaltParser
deriving instance IndentationParsing SaltParser

instance TokenParsing SaltParser where
  someSpace = SaltParser $ lift $ lift $ skipSome (void space <|> comment) where
    comment   = void (string "{-") *> inComment
    inComment =     void (string "-}")
                <|> skipSome (void (noneOf startEnd) <|> comment) *> inComment
                <|> oneOf startEnd *> inComment
    startEnd  = "{-}"

data SaltPState
  = SaltPState
    { _inputName :: String
    } deriving (Show)
makeLenses ''SaltPState

runSaltParser :: String -> SaltParser a -> Parser a
runSaltParser name p = evalIndentationParserT
                         (evalStateT (runSaltParser' p) pstate)
                         (mkIndentationState 0 infIndentation False Gt)
  where
    pstate = SaltPState name

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
conIdent = ident saltConStyle

reservedCon :: String -> SaltParser ()
reservedCon = reserve saltConStyle

varIdent :: SaltParser Name 
varIdent = ident saltVarStyle

tyVarIdent :: SaltParser TVName 
tyVarIdent = RName <$> ident saltVarStyle

reserved :: String -> SaltParser ()
reserved = reserve saltVarStyle

-- | Get line number from position
lineNum :: Delta -> Int
lineNum (Lines l _ _ _)      = fromIntegral l + 1
lineNum (Directed _ l _ _ _) = fromIntegral l + 1
lineNum _ = 0

-- | Get column number from position
columnNum :: Delta -> Int
columnNum pos = fromIntegral (column pos) + 1

-- | Get current position in source file.
srcPos :: DeltaParsing m => m (Row,Column)
srcPos = (lineNum &&& columnNum) <$> position

-- | Capture range of source file consumed by the parser.
captureSrcRef :: SaltParser a -> SaltParser (a, SrcRef)
captureSrcRef parse = do
  file <- use inputName
  start <- srcPos
  value <- parse
  end   <- srcPos
  return (value, SrcRef file start end)
  
