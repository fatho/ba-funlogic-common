{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
module Language.SaLT.Parser where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import qualified Data.Map                    as M
import           Text.Parser.Expression
import qualified Text.Parser.Token.Highlight as H
import           Text.Trifecta
import           Text.Trifecta.Indentation

import           Language.SaLT.AST
import           Language.SaLT.ParserDef     as P
import           FunLogic.Core.Parser        as P

parseSaltFileTest :: (MonadIO m) => FilePath -> m ()
parseSaltFileTest file = parseFromFile (runSaltParser file program) file >>= liftIO . print

parseSaltTest :: (MonadIO m, Show a) => SaltParser a -> String -> m ()
parseSaltTest p xs = do
  liftIO $ putStrLn xs
  liftIO $ putStrLn "-----------------------------------"
  parseTest (runSaltParser "<interactive>" p) xs


parseSaltFile :: (MonadIO m) => FilePath -> m (Maybe Module)
parseSaltFile file = parseFromFile (runSaltParser file program) file

program :: SaltParser Module
program =
  do
    decls <- whiteSpace *> many (absoluteIndentation decl) <* eof
    execStateT (mapM_ collectDecl decls) (emptyModule "Main")
  where
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
          forM_ _adtConstr $ \(ConDecl name args) ->
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
dataDecl = localIndentation Gt $ DData <$> adtParser

topLevelDecl :: SaltParser Decl
topLevelDecl = do
    ((name, ty, body), ref) <- captureSrcRef $ do
      (name, ty)    <- absoluteIndentation topLevelType
      (name', body) <- absoluteIndentation topLevelBody
      unless (name' == name) (fail "type declaration does not match body declaration")
      return (name, ty, body)
    return $ DTop name (Binding body ty ref)
  where
    topLevelType = (,) <$> varIdent <* symbol "::" <*> typeDecl <* optional semi
    topLevelBody = (,) <$> varIdent <* symbol "=" <*> expression

-- * Expression Parsing

opTable :: [[Operator SaltParser Exp]]
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
    consP x xs = ECon "Cons" [ty] [x, xs]
    nilP = ECon "Nil" [ty] []
  return $ foldr consP nilP list

-- * Pattern Parsing

patternP :: SaltParser Pat
patternP = choice
  [ PCon <$> conIdent <*> option [] (parens $ commaSep varIdent)
  , PCon "Pair" <$> parens ((\x y -> [x,y]) <$> varIdent <* comma <*> varIdent)
  , PVar <$> varIdent
  ]

-- * Helpers

annotBrackets :: SaltParser a -> SaltParser a
annotBrackets p = symbol "<:" *> p <* symbol ":>"
