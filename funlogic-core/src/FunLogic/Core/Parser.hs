{-# LANGUAGE ConstraintKinds #-}
module FunLogic.Core.Parser where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import qualified Data.HashSet                as HS
import qualified Text.Parser.Token.Highlight as H
import           Text.Trifecta
import           Text.Trifecta.Delta
import           Text.Trifecta.Indentation

import           FunLogic.Core.AST

type MonadicParsing m = (FileParsing m, IndentationParsing m, DeltaParsing m, TokenParsing m, CharParsing m, Parsing m)

class Parsing m => FileParsing m where
  fileName :: m String

adtParser :: MonadicParsing m => m ADT
adtParser = localIndentation Gt $ do
    reserved "data"
    captureSrcRef $ ADT
      <$> conIdent
      <*> many tyVarIdent
      <*  symbolic '='
      <*> body
  where
    body    = conDecl `sepBy1` symbol "|"
    conDecl = ConDecl <$> conIdent <*> many simpleType

-- * Indentifiers

varStyle :: MonadicParsing m => IdentifierStyle m
varStyle = IdentifierStyle
            { _styleName      = "identifier"
            , _styleStart     = lower
            , _styleLetter    = alphaNum <|> oneOf "_'"
            , _styleReserved = HS.fromList
                [ "failed", "unknown"
                , "forall", "case", "of"
                , "let", "in", "free"
                ]
            , _styleHighlight = H.Identifier
            , _styleReservedHighlight = H.ReservedIdentifier
            }

conStyle :: MonadicParsing m => IdentifierStyle m
conStyle = IdentifierStyle
            { _styleName      = "constructor"
            , _styleStart     = upper
            , _styleLetter    = alphaNum <|> oneOf "_'"
            , _styleReserved  = HS.empty
            , _styleHighlight = H.Constructor
            , _styleReservedHighlight = H.ReservedConstructor
            }

conIdent :: MonadicParsing m => m Name
conIdent = ident conStyle

reservedCon :: MonadicParsing m => String -> m ()
reservedCon = reserve conStyle

varIdent :: MonadicParsing m => m Name
varIdent = ident varStyle

tyVarIdent :: MonadicParsing m => m TVName
tyVarIdent = ident varStyle

reserved :: MonadicParsing m => String -> m ()
reserved = reserve varStyle

-- * Type Parsing

functionType ::MonadicParsing m =>  m Type
functionType = chainr1 complexType (TFun <$ symbol "->")

complexType :: MonadicParsing m => m Type
complexType = choice
  [ TCon <$> conIdent <*> try (many simpleType)
  , try simpleType
  ]

simpleType :: MonadicParsing m => m Type
simpleType = choice
    [ TVar <$> tyVarIdent
    , TCon <$> conIdent <*> pure []
    , TCon "List" . pure <$> brackets functionType
    , try $ symbol "(" >> TTup <$> functionType <* comma <*> functionType <* symbol ")"
    , parens (localIndentation Any functionType)
    ]

typeDecl :: MonadicParsing m => m TyDecl
typeDecl = TyDecl <$> option [] forallVars <*> option [] (try context) <*> functionType where
  forallVars = reserved "forall" *> many tyVarIdent <* symbol "."
  context    = ( parens (commaSep constraint)
                 <|> liftM pure constraint
               ) <* symbol "=>"
  constraint = TyConstraint <$> conIdent <*> tyVarIdent

-- * Common Functions

annotBrackets :: TokenParsing m => m a -> m a
annotBrackets p = symbol "<:" *> p <* symbol ":>"

optionalAnnotBrackets :: TokenParsing m => m a -> m (Maybe a)
optionalAnnotBrackets p = option Nothing (Just <$> annotBrackets p)

skipComments :: Parser ()
skipComments = skipSome (void space <|> comment) where
  comment   = void (string "{-") *> inComment
  inComment =     void (string "-}")
             <|> skipSome (void (noneOf startEnd) <|> comment) *> inComment
             <|> oneOf startEnd *> inComment
  startEnd  = "{-}"

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
captureSrcRef :: (FileParsing m, DeltaParsing m) => m (SrcRef -> a) -> m a
captureSrcRef parse = do
  fn <- fileName
  start <- srcPos
  value <- parse
  end   <- srcPos
  return $ value (SrcRef fn start end)
