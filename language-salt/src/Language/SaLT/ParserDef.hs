{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, FlexibleContexts, TemplateHaskell #-}
module Language.SaLT.ParserDef
  ( SaltParser
  , runSaltParser
  ) where

import           Control.Applicative
import           Control.Lens hiding (noneOf)
import           Control.Monad
import           Control.Monad.State
import           Text.Trifecta
import           Text.Trifecta.Indentation

import           FunLogic.Core.Parser

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

instance FileParsing SaltParser where
  fileName = use inputName