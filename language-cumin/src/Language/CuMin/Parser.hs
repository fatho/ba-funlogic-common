{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
module Language.CuMin.Parser where

import           Control.Applicative
import           Control.Lens                hiding (noneOf)
import           Control.Monad
import           Control.Monad.State
import qualified Data.Map                    as M
import           Text.Parser.Expression
import qualified Text.Parser.Token.Highlight as H
import           Text.Trifecta
import           Text.Trifecta.Indentation

import           FunLogic.Core.Parser        as P
import           Language.CuMin.AST

newtype CuMinParser a = CuMinParser { runCuMinParser' :: StateT CuMinPState (IndentationParserT Char Parser) a }
  deriving ( Functor, Applicative, Alternative, Monad, MonadPlus, MonadState CuMinPState)

deriving instance Parsing CuMinParser
deriving instance CharParsing CuMinParser
deriving instance DeltaParsing CuMinParser
deriving instance IndentationParsing CuMinParser

instance TokenParsing CuMinParser where
  someSpace = CuMinParser $ lift $ lift skipComments

data CuMinPState
  = CuMinPState
    { _inputName :: String
    } deriving (Show)
makeLenses ''CuMinPState

runCuMinParser :: String -> CuMinParser a -> Parser a
runCuMinParser name p = evalIndentationParserT
                         (evalStateT (runCuMinParser' p) pstate)
                         (mkIndentationState 0 infIndentation False Gt)
  where
    pstate = CuMinPState name

instance FileParsing CuMinParser where
  fileName = use inputName

parseCuMinFileTest :: (MonadIO m) => FilePath -> m ()
parseCuMinFileTest file = parseFromFile (runCuMinParser file program) file >>= liftIO . print

parseCuMinTest :: (MonadIO m, Show a) => CuMinParser a -> String -> m ()
parseCuMinTest p xs = do
  liftIO $ putStrLn xs
  liftIO $ putStrLn "-----------------------------------"
  parseTest (runCuMinParser "<interactive>" p) xs


parseCuMinFile :: (MonadIO m) => FilePath -> m (Maybe Module)
parseCuMinFile file = parseFromFile (runCuMinParser file program) file

program :: CuMinParser Module
program =
  do
    decls <- whiteSpace *> many (absoluteIndentation decl) <* eof
    execStateT (mapM_ collectDecl decls) (emptyModule "Main")
  where
    emptyModule name = Module
      { _modName = name
      , _modBinds = M.empty
      , _modADTs = M.empty
      }
    collectDecl d = case d of
      DTop binding -> let topName = binding ^. bindingName
        in modBinds `uses` M.member topName >>= \case
          True  -> fail ("top-level " ++ topName ++ "is declared more than once")
          False -> modBinds . at topName .= Just binding

      DData adt@ADT {..} -> modADTs `uses` M.member _adtName >>= \case
        True  -> fail ("ADT " ++ _adtName ++ " is declared more than once")
        False -> modADTs . at _adtName .= Just adt
        -- TODO: verify that no constructor name is defined more than once

-- * Declaration Parsing

decl :: CuMinParser Decl
decl = dataDecl <|> topLevelDecl

dataDecl :: CuMinParser Decl
dataDecl = localIndentation Gt $ DData <$> adtParser

topLevelDecl :: CuMinParser Decl
topLevelDecl =
    captureSrcRef $ do
      (name, ty)    <- absoluteIndentation topLevelType
      (name':args, body) <- absoluteIndentation topLevelBody
      unless (name' == name) (fail "type declaration does not match body declaration")
      return $ DTop . Binding name args body ty
  where
    topLevelType = (,) <$> varIdent <* symbol "::" <*> typeDecl <* optional semi
    topLevelBody = (,) <$> some varIdent <* symbol "=" <*> expression

-- * Expression Parsing

opTable :: [[Operator CuMinParser Exp]]
opTable = [ [ Infix (prim2 PrimAdd  <$ symbol "+")   AssocLeft ]
          , [ Infix (prim2 PrimEq   <$ symbol "==")  AssocLeft ]
          ]
  where
    prim2 p x y = EPrim p [x, y]

expression :: CuMinParser Exp
expression = buildExpressionParser opTable simpleExpr

simpleExpr :: CuMinParser Exp
simpleExpr = choice
  [ appE
  , caseE
  ]

verySimpleExpr :: CuMinParser Exp
verySimpleExpr = choice
  [ try funE
  , varE
  , conE
  , litE
  , listE
  , letE
  , parens expression
  ]

letE :: CuMinParser Exp
letE = do
    reserved "let"
    name <- varIdent
    def <- letFree <|> letExp
    reserved "in"
    body <- expression
    case def of
      Left ty -> return $ ELetFree name ty body
      Right e -> return $ ELet name e body
  where
    letFree = symbol "::" >> Left <$> complexType <* reserved "free"
    letExp  = symbol "=" >> Right <$> expression

appE :: CuMinParser Exp
appE = chainl1 verySimpleExpr (pure EApp)

funE :: CuMinParser Exp
funE = EFun <$> varIdent <*> annotBrackets (commaSep functionType)

varE :: CuMinParser Exp
varE = EVar <$> varIdent

conE :: CuMinParser Exp
conE = ECon
       <$> conIdent
       <*> option [] (annotBrackets $ commaSep functionType)

litE :: CuMinParser Exp
litE = ELit . LInt <$> highlight H.Number natural

caseE :: CuMinParser Exp
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

-- * Syntactic Sugar

-- | syntactic sugar: [a,b,...] --> Cons a (Cons b ...)
listE :: CuMinParser Exp
listE = do
  list <- brackets $ commaSep expression
  ty   <- annotBrackets functionType
  let
    consP x = EApp (EApp (ECon "Cons" [ty]) x)
    nilP = ECon "Nil" [ty]
  return $ foldr consP nilP list

-- * Pattern Parsing

patternP :: CuMinParser Pat
patternP = choice
  [ PCon <$> conIdent <*> many varIdent
  , PVar <$> varIdent
  ]
