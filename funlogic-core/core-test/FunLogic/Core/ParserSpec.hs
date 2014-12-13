module FunLogic.Core.ParserSpec where

import           FunLogic.Core.AST
import           FunLogic.Core.Parser

import           Control.Applicative
import           Test.Hspec
import           Text.Trifecta
import           Text.Trifecta.Indentation

typeP :: IndentParser Type
typeP = whiteSpace *> functionType <* eof

adtP :: IndentParser ADT
adtP = absoluteIndentation (whiteSpace *> adtParser <* eof)

parseTypeShouldBe :: (String, Type) -> Spec
parseTypeShouldBe (str,ty) = it ("parses `" ++ str ++ "` correctly") $
  case runParser "<test>" str typeP of
    Success ty' -> ty' `shouldBe` ty
    Failure msg -> expectationFailure $ show msg

parseADTShouldBe :: (String, ADT) -> Spec
parseADTShouldBe (str,adt) = it ("parses `" ++ str ++ "` correctly") $
  case runParser "<test>" str adtP of
    Success adt' -> adt' `shouldBe` adt
    Failure msg -> expectationFailure $ show msg

parseTypeShouldFail :: String -> Spec
parseTypeShouldFail str = it ("rejects `" ++ str ++ "` as expected") $
  case runParser "<test>" str typeP of
    Success ty -> expectationFailure $ show "Gave `" ++ show ty ++ "` instead of failure"
    Failure _ -> return ()

parseADTShouldFail :: String -> Spec
parseADTShouldFail str = it ("rejects `" ++ str ++ "` as expected") $
  case runParser "<test>" str adtP of
    Success adt -> expectationFailure $ show "Gave `" ++ show adt ++ "` instead of failure"
    Failure _ -> return ()

spec :: Spec
spec = do
    describe "parses types correctly" typeParseSpec
    describe "parses ADTs correctly" adtParseSpec

typeParseSpec :: Spec
typeParseSpec = do
  mapM_ parseTypeShouldBe
    [ ("Int", TCon "Int" [])
    , ("Either a b", TCon "Either" [TVar "a", TVar "b"])
    , ("a->b -> c", TCon "->" [TVar "a", TCon "->" [TVar "b", TVar "c"]])
    , ("(a ->b  ) -> c", TCon "->" [TCon "->" [TVar "a", TVar "b"], TVar "c"])
    , ("D(a->b)c -> d", TCon "->" [TCon "D" [TCon "->" [TVar "a", TVar "b"], TVar "c"], TVar "d"])
    ]
  mapM_ parseTypeShouldFail
    [ "a a"
    , "(a"
    , "a -> b )"
    , "?"
    , "a -> -> b"
    ]

adtParseSpec :: Spec
adtParseSpec = do
  mapM_ parseADTShouldBe
    [ ("data D = D", ADT "D" [] [ConDecl "D" []] undefined)
    , ("data List a = Nil | Cons a (List a)", ADT "List" ["a"] [ConDecl "Nil" [], ConDecl "Cons" [TVar "a", TCon "List" [TVar "a"]]] undefined)
    , ("data Tree type = Leaf(type)|Node(Tree type)(Tree(type))"
      , ADT "Tree" ["type"] [ConDecl "Leaf" [TVar "type"], ConDecl "Node" [TCon "Tree" [TVar "type"], TCon "Tree" [TVar "type"]]] undefined)
    ]
  mapM_ parseADTShouldFail
    [ "data = C"
    , "data D ="
    , "data a ="
    , "data D1_ a = a (D a)"
    , "data D = D -> D"
    , "data Dat = ()"
    , "data D = C |"
    , "data D =\nD" -- wrong indentation
    ]
