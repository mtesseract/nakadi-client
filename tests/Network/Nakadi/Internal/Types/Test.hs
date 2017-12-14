{-# LANGUAGE OverloadedStrings     #-}

module Network.Nakadi.Internal.Types.Test where

import ClassyPrelude
import Test.Tasty
import Test.Tasty.HUnit
import Data.Aeson
import Network.Nakadi.Types.Service

testTypes :: TestTree
testTypes = testGroup "Types"
  [ testService
  ]

testService :: TestTree
testService = testGroup "Service"
  [ testCase "JSON-decoding CursorCommitResults" testDecodeCursorCommitResults
  ]

testDecodeCursorCommitResults :: Assertion
testDecodeCursorCommitResults = assertBool "Failed to decode" $ isJust (decode sampleResponse :: (Maybe CursorCommitResults))
  where sampleResponse = "{\"items\":[{\"cursor\":{\"partition\":\"0\",\"offset\":\"001-0001-000000000000007598\",\"event_type\":\"http4s-nakadi.test-event\",\"cursor_token\":\"3bb3a590-ede5-43a9-981e-2bea26347c99\"},\"result\":\"outdated\"}]}"
