---
title: Examples
---

Dumping list of event types:

```haskell
dumpEventTypes :: Nakadi.ConfigIO -> LoggingT IO ()
dumpEventTypes config = Nakadi.runNakadiT config $ do
  eventTypes <- Nakadi.eventTypesList
  forM_ eventTypes $ \ eventType ->
    logInfoN $ tshow (eventType^.L.name)
```

Dumping subscription events:

```haskell
dumpSubscription :: (MonadLogger m, MonadNakadi IO m) => Nakadi.SubscriptionId -> m ()
dumpSubscription subscriptionId =
  Nakadi.subscriptionProcess Nothing subscriptionId processBatch

  where processBatch :: MonadLogger m => Nakadi.SubscriptionEventStreamBatch Value -> m ()
        processBatch batch =
          logInfoN (tshow batch)
```

Asyncronously republishing events belonging to one event type name
under a different event type name:

```haskell
runEcho :: EventTypeName -> EventTypeName -> NakadiT IO IO ()
runEcho eventNameInput eventNameOutput =
  runResourceT $ do
  channel :: TBQueue (Vector Value) <- atomically $ newTBQueue 1024
  consumer  <- async $ consumeEvents eventNameInput channel
  publisher <- async $ publishEvents eventNameOutput channel
  link consumer
  link publisher
  waitEither_ consumer publisher

  where consumeEvents eventName channel =
          eventsProcessConduit (Just consumeParameters) eventName Nothing $
          concatMapC (view L.events)
          .| mapC (fmap (toJSON :: DataChangeEvent Value -> Value))
          .| sinkTBQueue channel

        publishEvents eventName channel =
          sourceTBQueue channel
            .| mapC toList
            $$ mapM_C (eventsPublish eventName)

        consumeParameters = defaultConsumeParameters & L.batchFlushTimeout .~ Just 1
```
