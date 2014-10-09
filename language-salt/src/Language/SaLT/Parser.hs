{-# LANGUAGE TemplateHaskell, RankNTypes #-}
module Language.SaLT.Parser where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import qualified Data.HashSet as HS
import           Safe
import           Text.Parser.Expression
import qualified Text.Parser.Token.Highlight as H
import           Text.Trifecta
import           Text.Trifecta.Combinators
import           Text.Trifecta.Delta
import           Text.Trifecta.Indentation

import           Language.SaLT.AST
import           Language.SaLT.ParserDef as P

program :: SaltParser Program
program = whiteSpace >> Program
          <$> (many $ absoluteIndentation decl)
          <* eof
          
-- * Declaration Parsing

decl :: SaltParser Decl
decl = dataDecl <|> topLevelDecl

dataDecl :: SaltParser Decl
dataDecl = localIndentation Gt $ do
  reserved "data"
  DData
    <$> conIdent
    <*> many varIdent
    <*  symbolic '='
    <*> body
  where
    body    = conDecl `sepBy1` (symbol "|")
    conDecl = ConDecl <$> conIdent <*> many simpleType

topLevelDecl :: SaltParser Decl
topLevelDecl = do
    (name, ty)    <- absoluteIndentation topLevelType
    (name', body) <- absoluteIndentation topLevelBody
    unless (name' == name) (fail "type declaration does not match body declaration")
    return $ DTop name ty body
  where
    topLevelType = (,) <$> varIdent <* symbol "::" <*> typeDecl <* optional semi
    topLevelBody = (,) <$> varIdent <* symbol "=" <*> expression

-- * Type Parsing

functionType :: SaltParser Type
functionType = chainr1 complexType (TFun <$ symbol "->") 

complexType :: SaltParser Type 
complexType = choice
  [ TSet <$> (reservedCon "Set" *> simpleType)
  , TCon <$> conIdent <*> try (many simpleType)
  , try simpleType
  ]

simpleType :: SaltParser Type
simpleType = choice
    [ TVar <$> varIdent
    , TCon <$> conIdent <*> pure []
    , TCon "List" . pure <$> brackets functionType
    , parensType <$> parens (localIndentation Any $ commaSep functionType)
    ]
  where
    parensType [x] = x
    parensType xs = TTup xs

typeDecl :: SaltParser TyDecl
typeDecl = TyDecl <$> option [] forallVars <*> option [] (try context) <*> functionType where
  forallVars = reserved "forall" *> many varIdent <* symbol "."
  context    = ( parens (commaSep constraint)
                 <|> liftM pure constraint
               ) <* symbol "=>"
  constraint = TyConstraint <$> conIdent <*> varIdent

-- * Expression Parsing

opTable = [ [ Infix (prim2 PrimAdd <$ symbol "+") AssocLeft ]
          , [ Infix (prim2 PrimEq <$ symbol "==") AssocLeft ]
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
  , conE
  , litE
  , listE
  , tupOrParensE
  ]

appE :: SaltParser Exp
appE = chainl1 verySimpleExpr (pure EApp)

funE :: SaltParser Exp
funE = EFun <$> varIdent <*> annotBrackets (commaSep functionType)

varE :: SaltParser Exp
varE = EVar <$> varIdent

conE :: SaltParser Exp
conE = ECon
       <$> conIdent
       <*> option [] (annotBrackets $ commaSep functionType)
       <*> option [] (parens $ commaSep expression)

-- | syntactic sugar: [a,b,...] --> Cons(a,Cons(b,...))
listE :: SaltParser Exp
listE = do
  list <- brackets $ commaSep expression
  ty   <- annotBrackets functionType
  let
    cons x xs = ECon "Cons" [ty] [x, xs]
    nil = ECon "Nil" [ty] []
  return $ foldr cons nil list


tupOrParensE :: SaltParser Exp
tupOrParensE = do
  vals <- parens (commaSep expression)
  case vals of
    [x] -> return x
    xs  -> return $ ETup xs

litE :: SaltParser Exp
litE = ELit . LInt <$> highlight H.Number natural

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
    alts <- localIndentation Gt (some $ absoluteIndentation alt)
    return (ECase scrutinee alts)
  where
    alt = Alt <$> pattern <* symbol "->" <*> expression

-- * Pattern Parsing

pattern :: SaltParser Pat
pattern = choice [conP, varP, tupP]

conP = PCon <$> conIdent <*> option [] (parens $ commaSep varIdent)
tupP = PTup <$> parens (commaSep varIdent)
varP = PVar <$> varIdent


-- * Helpers

annotBrackets :: SaltParser a -> SaltParser a
annotBrackets p = symbol "<:" *> p <* symbol ":>"
