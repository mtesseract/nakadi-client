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

testSubscriptions :: Config App -> TestTree
testSubscriptions conf = testGroup "Subscriptions"
  [ testCase "SubscriptionsList" (testSubscriptionsList conf)
  , testCase "SubscriptionsCreateDelete" (testSubscriptionsCreateDelete conf)
  , testCase "SubscriptionDoubleDeleteFailure" (testSubscriptionsDoubleDeleteFailure conf)
  ]

testSubscriptionsList :: Config App -> Assertion
testSubscriptionsList conf = runApp . runNakadiT conf $ do
  -- Cleanup
  deleteSubscriptionsByAppPrefix prefix
  -- Create new Subscriptions
  maybeSubscriptionIds <- forM [1..n] $ \i -> do
    let owningApp = ApplicationName (prefix <> tshow i)
    subscription <- subscriptionCreate (mySubscription (Just owningApp))
    return (subscription^.L.id)
  let subscriptionIds = catMaybes maybeSubscriptionIds
  liftIO $ n @=? length subscriptionIds
  -- Retrieve list of all Subscriptions
  subscriptions' <- subscriptionsList Nothing Nothing
  -- Filter for the subscriptions we have created above
  let subscriptionsFiltered = filter (subscriptionAppHasPrefix prefix) subscriptions'
      subscriptionIdsFiltered = catMaybes . map (view L.id) $ subscriptionsFiltered
  liftIO $ n @=? length subscriptionIdsFiltered
  liftIO $ sort subscriptionIds @=? sort subscriptionIdsFiltered
  
  where n = 100
        prefix = "test-suite-list-"

deleteSubscriptionsByAppPrefix :: MonadNakadiEnv b m => Text -> m ()
deleteSubscriptionsByAppPrefix prefix = do
  subscriptions <- subscriptionsList Nothing Nothing
  let subscriptionsFiltered = filter (subscriptionAppHasPrefix prefix) subscriptions
      subscriptionIdsFiltered = catMaybes . map (view L.id) $ subscriptionsFiltered
  forM_ subscriptionIdsFiltered subscriptionDelete

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

testSubscriptionsCreateDelete :: Config App -> Assertion
testSubscriptionsCreateDelete conf = runApp . runNakadiT conf $ do
  subscription <- subscriptionCreate (mySubscription Nothing)
  liftIO $ True @=? isJust (subscription^.L.id)
  let (Just subscriptionId) = subscription^.L.id
  subscriptionDelete subscriptionId
  return ()

testSubscriptionsDoubleDeleteFailure :: Config App -> Assertion
testSubscriptionsDoubleDeleteFailure conf = runApp . runNakadiT conf $ do
  subscription <- subscriptionCreate (mySubscription Nothing)
  liftIO $ True @=? isJust (subscription^.L.id)
  let (Just subscriptionId) = subscription^.L.id
  subscriptionDelete subscriptionId
  res <- try (subscriptionDelete subscriptionId)
  case res of
    Left (SubscriptionNotFound _) -> return ()
    _ -> liftIO $ assertFailure "Expected SubscriptionNotFound exception"
  return ()
