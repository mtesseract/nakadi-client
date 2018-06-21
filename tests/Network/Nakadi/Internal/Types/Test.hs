{-# LANGUAGE OverloadedStrings     #-}

module Network.Nakadi.Internal.Types.Test where

import qualified Data.ByteString.Lazy          as LB
import           ClassyPrelude
import           Test.Tasty
import           Test.Tasty.HUnit
import           Data.Aeson
import           Network.Nakadi.Types.Service

testTypes :: TestTree
testTypes = testGroup "Types" [testService]

testService :: TestTree
testService = testGroup
  "Service"
  [ testCase "JSON Decode: CursorCommitResults" testDecodeCursorCommitResults
  , testCase "JSON Decode: SubscriptionRequest" testDecodeSubscriptionRequest
  ]

jsonEqual :: LB.ByteString -> LB.ByteString -> Bool
jsonEqual a b =
  let Just a' = decode a :: Maybe Value
      Just b' = decode b :: Maybe Value
  in  a' == b'

testDecodeSubscriptionRequest :: Assertion
testDecodeSubscriptionRequest = do
  let
    req pos = SubscriptionRequest
      { _owningApplication    = "test"
      , _eventTypes           = ["event1"]
      , _consumerGroup        = Nothing
      , _subscriptionPosition = pos
      }
    req'End
      = "\
         \{ \"owning_application\": \"test\" \
         \, \"event_types\": [\"event1\"] \
         \, \"read_from\": \"end\" \
         \}"
    req'Begin
      = "\
        \{ \"owning_application\": \"test\" \
        \, \"event_types\": [\"event1\"] \
        \, \"read_from\": \"begin\" \
        \}"
    req'Cursors
      = "\
        \{ \"owning_application\": \"test\" \
        \, \"event_types\": [\"event1\"] \
        \, \"read_from\": \"cursors\" \
        \, \"cursors\": [] \
        \}"
    cursors = []

  assertBool "Failed to serialize SubscriptionRequest with SubscriptionPositionEnd"
             (jsonEqual (encode (req (Just SubscriptionPositionEnd))) req'End)
  assertBool "Failed to deserialize SubscriptionRequest with SubscriptionPositionEnd"
             (decode req'End == Just (req (Just SubscriptionPositionEnd)))

  assertBool "Failed to serialize SubscriptionRequest with SubscriptionPositionBegin"
             (jsonEqual (encode (req (Just SubscriptionPositionBegin))) req'Begin)
  assertBool "Failed to deserialize SubscriptionRequest with SubscriptionPositionBegin"
             (decode req'Begin == Just (req (Just SubscriptionPositionBegin)))

  assertBool "Failed to serialize SubscriptionRequest with SubscriptionPositionCursors"
             (jsonEqual (encode (req (Just (SubscriptionPositionCursors cursors)))) req'Cursors)
  assertBool "Failed to deserialize SubscriptionRequest with SubscriptionPositionCursors"
             (decode req'Cursors == Just (req (Just (SubscriptionPositionCursors cursors))))

  assertBool "Failed to serialize SubscriptionRequest without SubscriptionPosition"
             (jsonEqual (encode (req Nothing)) req'End)

testDecodeCursorCommitResults :: Assertion
testDecodeCursorCommitResults = assertBool "Failed to decode"
  $ isJust (decode sampleResponse :: (Maybe CursorCommitResults))
 where
  sampleResponse
    = "{\"items\":[{\"cursor\":{\"partition\":\"0\",\"offset\":\"001-0001-000000000000007598\",\"event_type\":\"http4s-nakadi.test-event\",\"cursor_token\":\"3bb3a590-ede5-43a9-981e-2bea26347c99\"},\"result\":\"outdated\"}]}"
