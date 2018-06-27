{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Network.Nakadi.Internal.Types.Test where

import qualified Data.ByteString.Lazy          as LB
import           ClassyPrelude
import           Test.Tasty
import           Test.Tasty.HUnit
import           Data.Aeson
import           Network.Nakadi.Types.Service
import           Data.Aeson.QQ
import Data.Maybe (fromJust)

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
    req'End = encode [aesonQQ|{owning_application: "test", event_types: ["event1"], read_from: "end"}|]
    req'Begin = encode
      [aesonQQ|{owning_application: "test", event_types: ["event1"], read_from: "begin"}|]
    cursors = [] :: [SubscriptionCursorWithoutToken]
    req'Cursors
      = encode [aesonQQ|{owning_application: "test", event_types: ["event1"], read_from: "cursors", cursors: #{cursors}}|]

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
testDecodeCursorCommitResults = do
  let eventType = "http4s-nakadi.test-event"
      offset =  "001-0001-000000000000007598"
      cursorToken = "3bb3a590-ede5-43a9-981e-2bea26347c99"
      partition = "0"
      result = CursorCommitResultOutdated
      cursorCommitResultsJson
       = encode [aesonQQ|
          { items: [ { cursor: { partition: #{partition}
                               , offset: #{offset}
                               , event_type: #{eventType}
                               , cursor_token: #{cursorToken}
                               },
                       result: #{result}
                     }
                   ]
          }
        |]
      cursor = SubscriptionCursor { _partition = partition
                                  , _offset = offset
                                  , _eventType = eventType
                                  , _cursorToken = cursorToken}
      cursorCommitResults = CursorCommitResults { _items = [CursorCommitResult { _cursor = cursor , _result = result }] }
  assertBool "Failed to decode CursorsCommitResults" $
    fromJust (decode cursorCommitResultsJson) == cursorCommitResults
