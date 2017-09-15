{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}

module Network.Nakadi.Registry.Test where

import           ClassyPrelude

import           Network.Nakadi
import           Test.Tasty
import           Test.Tasty.HUnit

testRegistry :: Config -> TestTree
testRegistry conf = testGroup "Registry"
  [ testCase "PartitionStrategies" (testPartitionStrategies conf)
  ]

testPartitionStrategies :: Config -> Assertion
testPartitionStrategies conf = do
  void $ registryPartitionStrategies conf
