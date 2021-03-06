{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
module Language.CuMin.Parser
  (
  -- * simplified parser interface
    parseCuMinFile
  , parseCuMinFileEx
  , parseCuMinString
  -- * raw parser interface
  , CuMinParser
  , runCuMinParser
  , postProcessExp
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
import           Data.List                   (intercalate, nub, (\\))
import qualified Data.Monoid
import qualified Data.Set                    as Set
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

-- | Runs a CuMin parser naming the input.
runCuMinParser :: String -> CuMinParser a -> Parser a
runCuMinParser name p = evalIndentationParserT
                         (evalStateT (runCuMinParser' p) pstate)
                         (mkIndentationState 0 infIndentation False Gt)
  where
    pstate = CuMinPState name

instance FileParsing CuMinParser where
  fileName = use inputName

instance RunnableParsing CuMinParser where
  runParser name content parser = runParser name content $ runCuMinParser name parser

-- | Parses a CuMin program from a string returning detailed error information.
parseCuMinString :: String -> String -> Result [Decl]
parseCuMinString name = parseString (runCuMinParser name program) Data.Monoid.mempty

-- | Parses a CuMin file without detailed error messages in case of failure.
parseCuMinFile :: (MonadIO m) => FilePath -> m (Maybe [Decl])
parseCuMinFile file = parseFromFile (runCuMinParser file program) file

-- | Parses a CuMin file returning a detailled error message in case of failure.
parseCuMinFileEx :: (MonadIO m) => FilePath -> m (Result [Decl])
parseCuMinFileEx file = parseFromFileEx (runCuMinParser file program) file

-- | Parses a CuMin program as a list of declarations.
program :: CuMinParser [Decl]
program = whiteSpace *> many (absoluteIndentation decl) <* eof

-- * Declaration Parsing

-- | Parses a CuMin declaration (which may be a data or function declaration).
decl :: CuMinParser Decl
decl = dataDecl <|> topLevelDecl

-- | Parses a data declaration.
dataDecl :: CuMinParser Decl
dataDecl = localIndentation Gt $ DData <$> adtParser

-- | Parses a function definition.
topLevelDecl :: CuMinParser Decl
topLevelDecl = DTop <$> binding

-- | Parses a top level binding.
binding :: CuMinParser Binding
binding =
    captureSrcRef $ do
      (name, ty)    <- absoluteIndentation topLevelType
      (args, body) <- absoluteIndentation (topLevelBody name)
      return $ Binding name args body ty
  where
    topLevelType = (,) <$> varIdent <* symbol "::" <*> typeDecl <* optional semi
    topLevelBody name = do
      name' <- varIdent
      unless (name == name') $ fail "type declaration does not match body declaration"
      args <- many varIdent
      unless (nub args == args) $ fail $
              "The variable(s) "
              ++ intercalate ", " (map (\v -> "`" ++ v ++ "`") (args \\ nub args))
              ++ " occur(s) more than once on the left hand side of a binding"
      _ <- symbol "="
      expr <- expression
      return (args, postProcessExp (Set.fromList args) expr)

-- | Replaces every variable that is not in the given set of local variables
-- with a function with an empty type instantiation list.
postProcessExp :: Set.Set VarName -> Exp -> Exp
postProcessExp = go
  where
  go locals e = case e of
    -- This is the interesting part:
    EVar v -> if v `Set.member` locals then e else EFun v []
    -- The rest is just recursion
    ELet v x y -> ELet v (go locals x) (go (Set.insert v locals) y)
    ELetFree v ty x -> ELetFree v ty (go (Set.insert v locals) x)
    EFailed _ -> e
    EFun _ _ -> e
    EApp x y -> EApp (go locals x) (go locals y)
    ELit _ -> e
    EPrim oper es -> EPrim oper (map (go locals) es)
    ECon _ _ -> e
    ECase x alts -> ECase (go locals x) (map (postProcessAlt locals) alts)
  postProcessAlt locals (Alt pat e) = case pat of
    PVar v -> Alt pat (go (Set.insert v locals) e)
    PCon _ vs -> Alt pat (go (locals `Set.union` Set.fromList vs) e)

-- * Expression Parsing

opTable :: [[Operator CuMinParser Exp]]
opTable = [ [ Infix (prim2 PrimAdd  <$ symbol "+")   AssocLeft ]
          , [ Infix (prim2 PrimEq   <$ symbol "==")  AssocNone ]
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
  , failE
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
    letFree = -- allow function types while parsing for better error messages:
      symbol "::" >> Left <$> functionType <* reserved "free"
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
litE = ELit . LNat <$> highlight H.Number natural

failE :: CuMinParser Exp
failE = reserved "failed" >> EFailed <$> annotBrackets functionType

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

-- | Parses list syntactic sugar: [a,b,...] --> Cons a (Cons b ...)
listE :: CuMinParser Exp
listE = do
  list <- brackets $ commaSep expression
  ty   <- annotBrackets functionType
  let
    consP x = EApp (EApp (ECon "Cons" [ty]) x)
    nilP = ECon "Nil" [ty]
  return $ foldr consP nilP list

-- * Pattern Parsing

-- | Parses a case pattern.
patternP :: CuMinParser Pat
patternP = choice
  [ PCon <$> conIdent <*> many varIdent
  , PVar <$> varIdent
  ]
