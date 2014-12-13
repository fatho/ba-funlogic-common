{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
module Language.SaLT.Parser
  (
  -- * simplified parser interface
    parseSaltFile
  , parseSaltFileEx
  , parseSaltString
  -- * raw parser interface
  , SaltParser
  , runSaltParser
  , program
  , expression
  , binding
  , patternP
  , decl
  , dataDecl
  , topLevelDecl
  , module P
  ) where

import           Control.Applicative
import           Control.Lens                hiding (noneOf)
import           Control.Monad
import           Control.Monad.State
import qualified Data.Monoid
import           Text.Parser.Expression
import qualified Text.Parser.Token.Highlight as H
import           Text.Trifecta
import           Text.Trifecta.Indentation

import           FunLogic.Core.Parser        as P
import           Language.SaLT.AST

newtype SaltParser a = SaltParser { runSaltParser' :: StateT SaltPState (IndentationParserT Char Parser) a }
  deriving ( Functor, Applicative, Alternative, Monad, MonadPlus, MonadState SaltPState)

deriving instance Parsing SaltParser
deriving instance CharParsing SaltParser
deriving instance DeltaParsing SaltParser
deriving instance IndentationParsing SaltParser

instance TokenParsing SaltParser where
  someSpace = SaltParser $ lift $ lift skipComments

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

instance RunnableParsing SaltParser where
  runParser name content parser = runParser name content $ runSaltParser name parser

parseSaltString :: String -> String -> Result [Decl]
parseSaltString name = parseString (runSaltParser name program) Data.Monoid.mempty

parseSaltFile :: (MonadIO m) => FilePath -> m (Maybe [Decl])
parseSaltFile file = parseFromFile (runSaltParser file program) file

parseSaltFileEx :: (MonadIO m) => FilePath -> m (Result [Decl])
parseSaltFileEx file = parseFromFileEx (runSaltParser file program) file

program :: SaltParser [Decl]
program = whiteSpace *> many (absoluteIndentation decl) <* eof

-- * Declaration Parsing

decl :: SaltParser Decl
decl = dataDecl <|> topLevelDecl

dataDecl :: SaltParser Decl
dataDecl = localIndentation Gt $ DData <$> adtParser

topLevelDecl :: SaltParser Decl
topLevelDecl = DTop <$> binding

binding :: SaltParser Binding
binding = captureSrcRef $
  do
    (name, ty)    <- absoluteIndentation topLevelType
    (name', body) <- absoluteIndentation topLevelBody
    unless (name' == name) (fail "type declaration does not match body declaration")
    return $ Binding name body ty
  where
    topLevelType = (,) <$> varIdent <* symbol "::" <*> typeDecl <* optional semi
    topLevelBody = (,) <$> varIdent <* symbol "=" <*> expression

-- * Expression Parsing

opTable :: [[Operator SaltParser Exp]]
opTable = [ [ Infix (prim2 PrimAdd  <$ symbol "+")   AssocLeft ]
          , [ Infix (prim2 PrimEq   <$ symbol "==")  AssocNone ]
          , [ Infix (prim2 PrimBind <$ symbol ">>=") AssocLeft ]
          ]
  where
    prim2 p x y = EPrim p [x, y]

expression :: SaltParser Exp
expression = buildExpressionParser opTable simpleExpr

simpleExpr :: SaltParser Exp
simpleExpr = choice
  [ appE
  , caseE
  , lambdaE
  ]

verySimpleExpr :: SaltParser Exp
verySimpleExpr = choice
  [ try funE
  , varE
  , unknownE
  , failedE
  , conE
  , litE
  , listE
  , singletonSetE
  , parens expression
  ]

appE :: SaltParser Exp
appE = chainl1 verySimpleExpr (pure EApp)

funE :: SaltParser Exp
funE = EFun <$> varIdent <*> annotBrackets (commaSep functionType)

varE :: SaltParser Exp
varE = EVar <$> varIdent

unknownE :: SaltParser Exp
unknownE = reserved "unknown" >> EUnknown <$> annotBrackets functionType

failedE :: SaltParser Exp
failedE = reserved "unknown" >> EFailed <$> annotBrackets functionType

conE :: SaltParser Exp
conE = ECon
       <$> conIdent
       <*> option [] (annotBrackets $ commaSep functionType)

litE :: SaltParser Exp
litE = ELit . LNat <$> highlight H.Number natural

lambdaE :: SaltParser Exp
lambdaE = symbol "\\"
          >> ELam
          <$> varIdent
          <*  symbol "::"
          <*> complexType
          <*  symbol "->"
          <*> expression

caseE :: SaltParser Exp
caseE = do
    reserved "case"
    scrutinee <- expression
    reserved "of"
    as <- eAlts <|> iAlts
    return (ECase scrutinee as)
  where
    iAlts = localIndentation Gt (some $ absoluteIndentation alt)
    eAlts = symbol "{" *> localIndentation Any (alt `sepEndBy` semi) <* localIndentation Any (symbol "}")
    alt   = Alt <$> patternP <* symbol "->" <*> expression

singletonSetE :: SaltParser Exp
singletonSetE = ESet <$> braces expression

-- * Syntactic Sugar

-- | syntactic sugar: [a,b,...] --> Cons(a,Cons(b,...))
listE :: SaltParser Exp
listE = do
  list <- brackets $ commaSep expression
  ty   <- annotBrackets functionType
  let
    consP x = EApp (EApp (ECon "Cons" [ty]) x)
    nilP = ECon "Nil" [ty]
  return $ foldr consP nilP list

-- * Pattern Parsing

patternP :: SaltParser Pat
patternP = choice
  [ PCon <$> conIdent <*> many varIdent
  , PVar <$> varIdent
  ]
