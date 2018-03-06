{-# LANGUAGE FlexibleContexts #-}

module Network.Nakadi.Examples.Test where

import           ClassyPrelude
import qualified Network.Nakadi                            as Nakadi
import           Network.Nakadi.Examples.Subscription.Test
import           Test.Tasty
import           Test.Tasty.HUnit

testExamples :: Nakadi.Config IO -> TestTree
testExamples conf = testGroup "Examples"
  [ testCase "Subscription Consumption" (testConsumption conf)
  ]
