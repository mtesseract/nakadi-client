{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Network.Nakadi.Registry.Test where

import           ClassyPrelude

import           Network.Nakadi
import           Network.Nakadi.Tests.Common
import           Test.Tasty
import           Test.Tasty.HUnit

testRegistry :: Config App -> TestTree
testRegistry conf =
  testGroup "Registry" [testCase "PartitionStrategies" (testPartitionStrategies conf)]

testPartitionStrategies :: Config App -> Assertion
testPartitionStrategies conf = runApp . runNakadiT conf . void $ registryPartitionStrategies
