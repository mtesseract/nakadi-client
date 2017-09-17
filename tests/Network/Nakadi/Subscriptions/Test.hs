{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}

module Network.Nakadi.Subscriptions.Test where

import           ClassyPrelude

import           Control.Lens
import           Network.Nakadi
import qualified Network.Nakadi.Lenses       as L
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
testSubscriptionsList conf = do
  -- Cleanup
  deleteSubscriptionsByAppPrefix conf prefix
  -- Create new Subscriptions
  maybeSubscriptionIds <- forM [1..n] $ \i -> do
    let owningApp = ApplicationName (prefix <> tshow i)
    subscription <- subscriptionCreate conf (mySubscription (Just owningApp))
    return (subscription^.L.id)
  let subscriptionIds = catMaybes maybeSubscriptionIds
  n @=? length subscriptionIds
  -- Retrieve list of all Subscriptions
  subscriptions' <- subscriptionsList conf Nothing Nothing
  -- Filter for the subscriptions we have created above
  let subscriptionsFiltered = filter (subscriptionAppHasPrefix prefix) subscriptions'
      subscriptionIdsFiltered = catMaybes . map (view L.id) $ subscriptionsFiltered
  n @=? length subscriptionIdsFiltered
  sort subscriptionIds @=? sort subscriptionIdsFiltered

  where n = 100
        prefix = "test-suite-list-"

deleteSubscriptionsByAppPrefix :: Config -> Text -> IO ()
deleteSubscriptionsByAppPrefix conf prefix = do
  subscriptions <- subscriptionsList conf Nothing Nothing
  let subscriptionsFiltered = filter (subscriptionAppHasPrefix prefix) subscriptions
      subscriptionIdsFiltered = catMaybes . map (view L.id) $ subscriptionsFiltered
  forM_ subscriptionIdsFiltered (subscriptionDelete conf)

subscriptionAppHasPrefix :: Text -> Subscription -> Bool
subscriptionAppHasPrefix prefix subscription =
  let ApplicationName owningApp = subscription^.L.owningApplication
  in take (length prefix) owningApp == prefix

mySubscription :: Maybe ApplicationName -> Subscription
mySubscription maybeOwningApp = Subscription
  { _id = Nothing
  , _owningApplication = fromMaybe "test-suite" maybeOwningApp
  , _eventTypes = [myEventTypeName]
  , _consumerGroup = Nothing -- ??
  , _createdAt = Nothing
  , _readFrom = Just SubscriptionPositionEnd
  , _initialCursors = Nothing
  }

testSubscriptionsCreateDelete :: Config -> Assertion
testSubscriptionsCreateDelete conf = do
  subscription <- subscriptionCreate conf (mySubscription Nothing)
  True @=? isJust (subscription^.L.id)
  let (Just subscriptionId) = subscription^.L.id
  subscriptionDelete conf subscriptionId
  return ()

testSubscriptionsDoubleDeleteFailure :: Config -> Assertion
testSubscriptionsDoubleDeleteFailure conf = do
  subscription <- subscriptionCreate conf (mySubscription Nothing)
  True @=? isJust (subscription^.L.id)
  let (Just subscriptionId) = subscription^.L.id
  subscriptionDelete conf subscriptionId
  res <- try (subscriptionDelete conf subscriptionId)
  case res of
    Left (SubscriptionNotFound _) -> return ()
    _ -> assertFailure "Expected SubscriptionNotFound exception"
  return ()
