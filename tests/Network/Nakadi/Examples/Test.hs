{-# LANGUAGE FlexibleContexts #-}

module Network.Nakadi.Examples.Test where

import           ClassyPrelude
import qualified Network.Nakadi                    as Nakadi
import           Network.Nakadi.Examples.Echo.Test
import           Test.Tasty
import           Test.Tasty.HUnit

testExamples :: Nakadi.Config IO -> TestTree
testExamples conf = testGroup "Examples"
  [ testCase "Echo" (testEcho conf) ]
