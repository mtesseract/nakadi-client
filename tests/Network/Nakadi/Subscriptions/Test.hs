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
import qualified Network.Nakadi.Lenses         as L
import           Network.Nakadi.Tests.Common
import           Test.Tasty
import           Test.Tasty.HUnit
import           Network.Nakadi.Subscriptions.Processing.Test
import           Network.Nakadi.Subscriptions.Stats.Test

testSubscriptions :: Config App -> TestTree
testSubscriptions conf = testGroup
  "Subscriptions"
  [ testSubscriptionsStats conf
  , testSubscriptionsProcessing conf
  , testCase "SubscriptionsList"         (testSubscriptionsList conf)
  , testCase "SubscriptionsCreateDelete" (testSubscriptionsCreateDelete conf)
  , testCase "SubscriptionDoubleDeleteFailure"
             (testSubscriptionsDoubleDeleteFailure conf)
  ]

testSubscriptionsList :: Config App -> Assertion
testSubscriptionsList conf = run $ do
  -- Cleanup
  deleteSubscriptionsByAppPrefix prefix
  recreateEvent myEventType
  -- Create new Subscriptions
  subscriptionIds <- forM [1 .. n] $ \i -> do
    let owningApp = ApplicationName (prefix <> tshow i)
    subscription <- subscriptionCreate (mySubscription (Just owningApp))
    return (subscription ^. L.id)
  liftIO $ n @=? length subscriptionIds
  -- Retrieve list of all Subscriptions
  subscriptions' <- subscriptionsList Nothing Nothing
  -- Filter for the subscriptions we have created above
  let subscriptionsFiltered =
        filter (subscriptionAppHasPrefix prefix) subscriptions'
      subscriptionIdsFiltered = map (view L.id) $ subscriptionsFiltered
  liftIO $ n @=? length subscriptionIdsFiltered
  liftIO $ sort subscriptionIds @=? sort subscriptionIdsFiltered
 where
  n      = 100
  prefix = "test-suite-list-"
  run    = runApp . runNakadiT conf

deleteSubscriptionsByAppPrefix :: MonadNakadi b m => Text -> m ()
deleteSubscriptionsByAppPrefix prefix = do
  subscriptions <- subscriptionsList Nothing Nothing
  let subscriptionsFiltered =
        filter (subscriptionAppHasPrefix prefix) subscriptions
      subscriptionIdsFiltered = map (view L.id) $ subscriptionsFiltered
  forM_ subscriptionIdsFiltered subscriptionDelete

subscriptionAppHasPrefix :: Text -> Subscription -> Bool
subscriptionAppHasPrefix prefix subscription =
  let ApplicationName owningApp = subscription ^. L.owningApplication
  in  take (length prefix) owningApp == prefix

mySubscription :: Maybe ApplicationName -> SubscriptionRequest
mySubscription maybeOwningApp = SubscriptionRequest
  { _owningApplication    = fromMaybe "test-suite" maybeOwningApp
  , _eventTypes           = [myEventTypeName]
  , _consumerGroup        = Nothing
  , _subscriptionPosition = Nothing
  }

testSubscriptionsCreateDelete :: Config App -> Assertion
testSubscriptionsCreateDelete conf = runApp . runNakadiT conf $ do
  recreateEvent myEventType
  subscription <- subscriptionCreate (mySubscription Nothing)
  subscriptionDelete (subscription ^. L.id)
  return ()

testSubscriptionsDoubleDeleteFailure :: Config App -> Assertion
testSubscriptionsDoubleDeleteFailure conf = runApp . runNakadiT conf $ do
  recreateEvent myEventType
  subscription <- subscriptionCreate (mySubscription Nothing)
  subscriptionDelete (subscription ^. L.id)
  res <- try (subscriptionDelete (subscription ^. L.id))
  case res of
    Left (SubscriptionNotFound _) -> return ()
    _ -> liftIO $ assertFailure "Expected SubscriptionNotFound exception"
  return ()
