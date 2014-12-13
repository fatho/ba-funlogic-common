module FunLogic.Core.PrettySpec where

import           FunLogic.Core.AST
import           FunLogic.Core.Parser
import           FunLogic.Core.Pretty

import           Control.Applicative
import           Test.Hspec
import           Test.QuickCheck
import           Text.Trifecta
import qualified Text.Trifecta             as Trifecta
import           Text.Trifecta.Indentation

typeP :: IndentParser Type
typeP = whiteSpace *> functionType <* eof

adtP :: IndentParser ADT
adtP = absoluteIndentation (whiteSpace *> adtParser <* eof)

-- These generators are only useful for parsing and pretty printing.
-- They will not produce code that type checks.

adts :: Gen ADT
adts = do
  name <- upperName
  tyArgs <- shortListOf lowerName
  conDecls <- listOf1 condecls
  let srcRef = SrcRef "" (0,0) (0,0)
  return $ ADT name tyArgs conDecls srcRef

names :: Gen Name
names = do
  m <- elements [0..3]
  vectorOf m $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"

upperName :: Gen Name
upperName = do
  firstChar <- elements ['A'..'Z']
  (firstChar:) <$> names

lowerName :: Gen Name
lowerName =
  (do
    firstChar <- elements ['a'..'z']
    (firstChar:) <$> names
  ) `suchThat`
  (`notElem` ["forall", "let", "case", "of", "in", "free", "unknown", "failed"])

condecls :: Gen ConDecl
condecls = do
  name <- upperName
  ts <- shortListOf types
  return $ ConDecl name ts

types :: Gen Type
types = sized go where
  go 0 = oneof [TVar <$> lowerName, TCon <$> upperName <*> pure []]
  go n | n > 4 = go 4 -- otherwise the tests take forever
       | otherwise = oneof
    [ TCon "->" <$> sequence [go (n-1), go (n-1)]
    , TCon <$> upperName <*> shortListOf (go (n-1))
    , TVar <$> lowerName
    ]

shortListOf :: Gen a -> Gen [a]
shortListOf gen = do
  m <- elements [0..4]
  vectorOf m gen

spec :: Spec
spec = do
  it "pretty printing types is right inverse to parsing" $
    property $ forAll types $ \t -> case runParser "<test>" (show . removeFormatting $ prettyType t) typeP of
      Trifecta.Success t' -> t == t'
      Trifecta.Failure _ -> False
  it "pretty printing ADTs is right inverse to parsing" $
    property $ forAll adts $ \a -> case runParser "<test>" (show . removeFormatting $ prettyADT a) adtP of
      Trifecta.Success a' -> a == a'
      Trifecta.Failure _ -> False
