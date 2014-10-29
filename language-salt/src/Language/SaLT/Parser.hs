{-# LANGUAGE TemplateHaskell, RankNTypes, LambdaCase, RecordWildCards #-}
module Language.SaLT.Parser where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import qualified Data.HashSet as HS
import qualified Data.Map as M
import           Safe hiding (at)
import           Text.Parser.Expression
import qualified Text.Parser.Token.Highlight as H
import           Text.Trifecta
import           Text.Trifecta.Combinators
import           Text.Trifecta.Delta
import           Text.Trifecta.Indentation

import           Language.SaLT.AST
import           Language.SaLT.ParserDef as P

parseSaltFile :: (MonadIO m) => FilePath -> m ()
parseSaltFile file = parseFromFile (runSaltParser program) file >>= liftIO . print

program :: SaltParser Module
program = whiteSpace *> (many $ absoluteIndentation decl) <* eof >>= \ds -> execStateT (mapM_ collectDecl ds) (emptyModule "Main") where
  emptyModule name = Module
                     { _modName = name
                     , _modBinds = M.empty
                     , _modADTs = M.empty
                     , _modConstr = M.empty
                     }
  collectDecl d = case d of
    DTop topName binding -> modBinds `uses` M.member topName >>= \case
      True  -> fail ("top-level " ++ topName ++ "is declared more than once")
      False -> modBinds . at topName .= Just binding

    DData adt@ADT {..} -> modADTs `uses` M.member _adtName >>= \case
      True  -> fail ("ADT " ++ _adtName ++ " is declared more than once")
      False -> do
        modADTs . at _adtName .= Just adt
        forM_ (_adtConstr) $ \con@(ConDecl name args) -> do
          modConstr `uses` M.member name >>= \case
            True  -> fail ("constructor " ++ name ++ " is declared more than once")
            False -> do
              -- construct type from arguments
              let conTy = foldr TFun (TCon _adtName (map TVar _adtTyArgs)) args
              modConstr . at name .= Just (TyDecl _adtTyArgs [] conTy)

-- * Declaration Parsing

decl :: SaltParser Decl
decl = dataDecl <|> topLevelDecl

dataDecl :: SaltParser Decl
dataDecl = localIndentation Gt $ DData <$> do
  reserved "data"
  ADT
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
    return $ DTop name (Binding body ty)
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
    , TSet <$> braces functionType
    , TCon <$> conIdent <*> pure []
    , TCon "List" . pure <$> brackets functionType
    , try $ symbol "(" >> TTup <$> functionType <* comma <*> functionType <* symbol ")"
    , parens (localIndentation Any $ functionType)
    ]

typeDecl :: SaltParser TyDecl
typeDecl = TyDecl <$> option [] forallVars <*> option [] (try context) <*> functionType where
  forallVars = reserved "forall" *> many varIdent <* symbol "."
  context    = ( parens (commaSep constraint)
                 <|> liftM pure constraint
               ) <* symbol "=>"
  constraint = TyConstraint <$> conIdent <*> varIdent

-- * Expression Parsing

opTable = [ [ Infix (prim2 PrimAdd  <$ symbol "+")   AssocLeft ]
          , [ Infix (prim2 PrimEq   <$ symbol "==")  AssocLeft ]
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
  , conE
  , litE
  , listE
  , singletonSetE
  , try pairE
  , parens expression
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

pairE :: SaltParser Exp
pairE = symbol "(" >> EPair <$> expression <* comma <*> expression <* symbol ")"

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

singletonSetE :: SaltParser Exp
singletonSetE = ESet <$> braces expression

-- * Syntactic Sugar

-- | syntactic sugar: [a,b,...] --> Cons(a,Cons(b,...))
listE :: SaltParser Exp
listE = do
  list <- brackets $ commaSep expression
  ty   <- annotBrackets functionType
  let
    cons x xs = ECon "Cons" [ty] [x, xs]
    nil = ECon "Nil" [ty] []
  return $ foldr cons nil list

-- * Pattern Parsing

pattern :: SaltParser Pat
pattern = choice [conP, varP, tupP]

conP = PCon <$> conIdent <*> option [] (parens $ commaSep varIdent)
tupP = PTup <$> parens (commaSep varIdent)
varP = PVar <$> varIdent


-- * Helpers

annotBrackets :: SaltParser a -> SaltParser a
annotBrackets p = symbol "<:" *> p <* symbol ":>"
