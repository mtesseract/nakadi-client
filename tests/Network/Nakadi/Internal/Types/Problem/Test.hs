{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Network.Nakadi.Internal.Types.Problem.Test where

import           ClassyPrelude
import           Test.Tasty
import           Test.Tasty.HUnit
import           Data.Aeson
import           Network.Nakadi
import           Data.Aeson.QQ
import           Data.Maybe                     ( fromJust )

testProblem :: TestTree
testProblem = testGroup
    "Types/Problem"
    [ testCase "Minimal Deserialization"  testMinimalProblemDeserialization
    , testCase "Complete Deserialization" testCompleteProblemDeserialization
    , testCase "Custom Deserialization"   testCustomProblemDeserialization
    ]

testMinimalProblemDeserialization :: Assertion
testMinimalProblemDeserialization = assertBool "Minimal Problem can be deserialized"
                                               (problemJSON == toJSON problem)
  where
    problemJSON = [aesonQQ|{"title": "Something went wrong"}|] :: Value
    problemBS   = encode problemJSON
    problem     = fromJust (decode problemBS) :: Problem

testCompleteProblemDeserialization :: Assertion
testCompleteProblemDeserialization = assertBool "Complete Problem can be deserialized"
                                                (problemJSON == toJSON problem)
  where
    problemJSON =
        [aesonQQ|{"title": "Something went wrong",
                  "type": "/some/type/uri",
                  "status": 500,
                  "detail": "Server is dusty",
                  "instance": "https://another/uri/"}|] :: Value
    problemBS = encode problemJSON
    problem   = fromJust (decode problemBS) :: Problem

testCustomProblemDeserialization :: Assertion
testCustomProblemDeserialization = assertBool "Custom Problem can be deserialized"
                                              (problemJSON == toJSON problem)
  where
    problemJSON
        = [aesonQQ|{"title": "Something went wrong", "type": "/some/type/uri", "answer": 42}|] :: Value
    problemBS = encode problemJSON
    problem   = fromJust (decode problemBS) :: Problem
