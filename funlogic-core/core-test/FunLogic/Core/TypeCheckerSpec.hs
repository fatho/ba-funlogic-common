{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE QuasiQuotes #-}
module FunLogic.Core.TypeCheckerSpec where

import qualified Data.Map                  as M
import qualified Data.Set                  as S

import           FunLogic.Core.AST
import           FunLogic.Core.TH
import           FunLogic.Core.TypeChecker

import           Test.Hspec

dataConstraintsShouldBe :: [ADT] -> [(Name, [Int])] -> Expectation
dataConstraintsShouldBe adts constraints =
  deriveDataInstances adts' M.empty `shouldBe` constraints'
  where
    adts' = M.fromList $ map (\a -> (_adtName a, a)) adts
    constraints' = M.fromList $ map (\(n,s) -> (n, S.fromList s)) constraints

spec :: Spec
spec =
  describe "derives correct data constraints" $
    mapM_ checkConstraints
      [ ( return [adt|data Phantom a = P |]
        , [("Phantom", [])]
        )
      , ( return [adt|data List a = N | C a (List a) |]
        , [("List", [0])]
        )
      , ( return [adt|data Either a b = L a | R b |]
        , [("Either", [0,1])]
        )
      , ( return [adt|data NoData a = C (a -> a) |]
        , []
        )
      , ( [ [adt|data Phantom a = P |]
          , [adt|data IsData a = C (Phantom (a -> a))|]
          ]
        , [ ("Phantom", [])
          , ("IsData", [])
          ]
        )
      , ( [ [adt|data Rec1 a b = R1 a (Rec2 a b) |]
          , [adt|data Rec2 a b = R2 b (Rec1 b a) |]
          ]
        , [ ("Rec1", [0,1])
          , ("Rec2", [0,1])
          ]
        )
      , ( [ [adt| data Phantom a = P |]
          , [adt| data Either a b = L a | R b |]
          , [adt|
            data DataTest a b c d
              = C1 (Phantom a)
              | C2 b c
              | C3 (Phantom (a -> b))
              | C4 (DataTest a d c b)
            |]
          ]
        , [ ("Phantom", [])
          , ("Either", [0,1])
          , ("DataTest", [1,2,3])
          ]
        )
      ]
  where
    checkConstraints (adts, expectedConstraints) =
      it ("derives correct data constraints for " ++ show (map _adtName adts)) $
        adts `dataConstraintsShouldBe` expectedConstraints
