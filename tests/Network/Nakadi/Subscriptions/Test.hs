{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}

module Network.Nakadi.Subscriptions.Test where

import           ClassyPrelude

import           Network.Nakadi
import           Network.Nakadi.Tests.Common
import           Test.Tasty
import           Test.Tasty.HUnit

testSubscriptions :: Config -> TestTree
testSubscriptions conf = testGroup "Subscriptions"
  [ testCase "SubscriptionsList" (testSubscriptionsList conf)
  , testCase "SubscriptionsCreateDelete" (testSubscriptionsCreateDelete conf)
  , testCase "SubscriptionDoubleDeleteFailure" (testSubscriptionsDoubleDeleteFailure conf)
  ]

testSubscriptionsList :: Config -> Assertion
testSubscriptionsList conf =
  void $ subscriptionsList conf Nothing Nothing

mySubscription :: Subscription
mySubscription = Subscription
  { _id = Nothing
  , _owningApplication = "test-suite"
  , _eventTypes = [myEventTypeName]
  , _consumerGroup = Nothing -- ??
  , _createdAt = Nothing
  , _readFrom = Just SubscriptionPositionEnd
  , _initialCursors = Nothing
  }

testSubscriptionsCreateDelete :: Config -> Assertion
testSubscriptionsCreateDelete conf = do
  subscription <- subscriptionCreate conf mySubscription
  True @=? isJust (_id subscription)
  let (Just subscriptionId) = _id subscription
  subscriptionDelete conf subscriptionId
  return ()

testSubscriptionsDoubleDeleteFailure :: Config -> Assertion
testSubscriptionsDoubleDeleteFailure conf = do
  subscription <- subscriptionCreate conf mySubscription
  True @=? isJust (_id subscription)
  let (Just subscriptionId) = _id subscription
  subscriptionDelete conf subscriptionId
  res <- try (subscriptionDelete conf subscriptionId)
  case res of
    Left (SubscriptionNotFound _) -> return ()
    _ -> assertFailure "Expected SubscriptionNotFound exception"
  return ()

