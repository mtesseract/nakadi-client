{-|
Module      : Network.Nakadi.Subscriptions.Events
Description : Implementation of Nakadi Subscription Events API
Copyright   : (c) Moritz Schulte 2017, 2018
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX

This module implements a high level interface for the
@\/subscriptions\/SUBSCRIPTIONS\/events@ API.
-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Network.Nakadi.Subscriptions.Events
  ( subscriptionProcessConduit
  , subscriptionProcess
  ) where

import           Network.Nakadi.Internal.Prelude

import           Conduit                              hiding (throwM)
import qualified Control.Concurrent.Async.Timer       as Timer
import           Control.Concurrent.STM               (TBQueue, TVar,
                                                       atomically, modifyTVar,
                                                       newTBQueue, newTVar,
                                                       readTBQueue, readTVar,
                                                       retry, swapTVar,
                                                       writeTBQueue, writeTVar)
import           Control.Lens
import           Control.Monad.IO.Unlift
import           Control.Monad.Logger
import           Data.Aeson
import qualified Data.Conduit.List                    as Conduit (mapM_)
import           Data.HashMap.Strict                  (HashMap)
import qualified Data.HashMap.Strict                  as HashMap
import           Data.List                            (partition)
import qualified Data.Vector                          as Vector
import           Network.HTTP.Client                  (responseBody)
import           Network.HTTP.Simple
import           Network.HTTP.Types
import           Network.Nakadi.Internal.Config
import           Network.Nakadi.Internal.Conversions
import           Network.Nakadi.Internal.Http
import qualified Network.Nakadi.Internal.Lenses       as L
import           Network.Nakadi.Subscriptions.Cursors
import           UnliftIO.Async

-- | Consumes the specified subscription using the commit strategy
-- contained in the configuration. Each consumed batch of subscription
-- events is provided to the provided batch processor action. If this
-- action throws an exception, subscription consumption will terminate.
subscriptionProcess
  :: ( MonadNakadi b m
     , MonadUnliftIO m
     , MonadMask m
     , FromJSON a )
  => Maybe ConsumeParameters
  -> SubscriptionId                           -- ^ Subscription to consume
  -> (SubscriptionEventStreamBatch a -> m ()) -- ^ Batch processor action
  -> m ()
subscriptionProcess maybeConsumeParameters subscriptionId processor =
  subscriptionProcessConduit maybeConsumeParameters subscriptionId conduit
  where conduit = iterMC processor

-- | Conduit based interface for subscription consumption. Consumes
-- the specified subscription using the commit strategy contained in
-- the configuration. Each consumed event batch is then processed by
-- the provided conduit. If an exception is thrown inside the conduit,
-- subscription consumption will terminate.
subscriptionProcessConduit
  :: ( MonadNakadi b m
     , MonadUnliftIO m
     , MonadMask m
     , FromJSON a )
  => Maybe ConsumeParameters
  -> SubscriptionId
  -> ConduitM (SubscriptionEventStreamBatch a) (SubscriptionEventStreamBatch a) m ()
  -> m ()
subscriptionProcessConduit maybeConsumeParameters subscriptionId processor = do
  config <- nakadiAsk
  let consumeParams = fromMaybe defaultConsumeParameters maybeConsumeParameters
      queryParams   = buildSubscriptionConsumeQueryParameters consumeParams
  httpJsonBodyStream ok200 [(status404, errorSubscriptionNotFound)]
    (includeFlowId config
     . setRequestPath path
     . setRequestQueryParameters queryParams) $
    subscriptionProcessHandler config subscriptionId processor

  where path = "/subscriptions/"
               <> subscriptionIdToByteString subscriptionId
               <> "/events"

-- | Derive a 'SubscriptionEventStream' from the provided
-- 'SubscriptionId' and Nakadi streaming response.
buildSubscriptionEventStream
  :: MonadThrow m
  => SubscriptionId
  -> Response a
  -> m SubscriptionEventStream
buildSubscriptionEventStream subscriptionId response =
  case listToMaybe (getResponseHeader "X-Nakadi-StreamId" response) of
    Just streamId ->
      pure SubscriptionEventStream
      { _streamId       = StreamId (decodeUtf8 streamId)
      , _subscriptionId = subscriptionId }
    Nothing ->
      throwM StreamIdMissing

subscriptionProcessHandler
  :: ( MonadNakadi b m
     , MonadUnliftIO m
     , MonadMask m
     , FromJSON a )
  => Config b
  -> SubscriptionId
  -> ConduitM (SubscriptionEventStreamBatch a) (SubscriptionEventStreamBatch a) m ()
  -> Response (ConduitM () ByteString m ())
  -> m ()
subscriptionProcessHandler config subscriptionId processor response = do
  eventStream <- buildSubscriptionEventStream subscriptionId response
  let producer = responseBody response
                 .| linesUnboundedAsciiC
                 .| conduitDecode config
                 .| processor
  case config^.L.commitStrategy of
    CommitSync ->
      runConduit $ producer .| subscriptionSink eventStream
    CommitAsync bufferingStrategy -> do
      queue <- liftIO . atomically $ newTBQueue 1024
      withAsync (subscriptionCommitter bufferingStrategy eventStream queue) $
        \ asyncHandle -> do
          link asyncHandle
          runConduit $ producer .| Conduit.mapM_ (sendToQueue queue)

  where sendToQueue queue batch = liftIO . atomically $ do
          let cursor  = batch^.L.cursor
              events  = fromMaybe Vector.empty (batch^.L.events)
              nEvents = length events
          writeTBQueue queue (nEvents, cursor)

-- | Sink which can be used as sink for Conduits processing
-- subscriptions events. This sink takes care of committing events. It
-- can consume any values which contain Subscription Cursors.
subscriptionSink
  :: (MonadIO m, MonadNakadi b m)
  => SubscriptionEventStream
  -> ConduitM (SubscriptionEventStreamBatch a) void m ()
subscriptionSink eventStream = do
  config <- lift nakadiAsk
  awaitForever $ \ batch -> lift $ do
    let cursor  = batch^.L.cursor
    catchAny (subscriptionCursorCommit eventStream [cursor]) $ \ exn -> nakadiLiftBase $
      case config^.L.logFunc of
        Just logFunc -> logFunc "nakadi-client" LevelWarn $ toLogStr $
          "Failed to synchonously commit cursor: " <> tshow exn
        Nothing ->
          pure ()

subscriptionCommitter
  :: ( MonadNakadi b m
     , MonadUnliftIO m
     , MonadMask m )
  => CommitBufferingStrategy
  -> SubscriptionEventStream
  -> TBQueue (Int, SubscriptionCursor)
  -> m ()

subscriptionCommitter CommitNoBuffer eventStream queue = loop
  where loop = do
          config <- nakadiAsk
          (_, cursor) <- liftIO . atomically . readTBQueue $ queue
          catchAny (subscriptionCursorCommit eventStream [cursor]) $ \ exn -> do
            liftIO . putStrLn $ "Exception: " <> show exn
            nakadiLiftBase $
              case config^.L.logFunc of
                Just logFunc -> logFunc "nakadi-client" LevelWarn $ toLogStr $
                  "Failed to commit cursor " <> tshow cursor <> ": " <> tshow exn
                Nothing ->
                  pure ()
          loop

subscriptionCommitter (CommitTimeBuffer millis) eventStream queue = do
  let timerConf = Timer.defaultConf
                  & Timer.setInitDelay (fromIntegral millis)
                  & Timer.setInterval  (fromIntegral millis)
  cursorsMap <- liftIO . atomically $ newTVar HashMap.empty
  withAsync (cursorConsumer cursorsMap) $ \ asyncCursorConsumer -> do
    link asyncCursorConsumer
    Timer.withAsyncTimer timerConf $ cursorCommitter cursorsMap

  where -- The cursorsConsumer drains the cursors queue and adds each
        -- cursor to the provided cursorsMap.
        cursorConsumer cursorsMap = loop
          where loop = do
                  _cursor <- liftIO . atomically $ do
                    (_, cursor) <- readTBQueue queue
                    modifyTVar cursorsMap (HashMap.insert (cursor^.L.partition) (0, cursor))
                    pure cursor
                  loop

        -- The cursorsCommitter is responsible for periodically committing
        -- the cursors in the provided cursorsMap.
        cursorCommitter cursorsMap timer = loop
          where loop = do
                  Timer.wait timer
                  commitAllCursors eventStream cursorsMap
                  loop

subscriptionCommitter CommitSmartBuffer eventStream queue = do
  config <- nakadiAsk
  let millis            = 1000
      nMaxEventsDefault = 1000
      consumeParameters = fromMaybe defaultConsumeParameters (config^.L.consumeParameters)
      nMaxEvents        = fromIntegral $ (fromMaybe nMaxEventsDefault (consumeParameters^.L.maxUncommittedEvents)) `div` 2

      timerConf = Timer.defaultConf
                  & Timer.setInitDelay (fromIntegral millis)
                  & Timer.setInterval  (fromIntegral millis)
  cursorsMap <- liftIO . atomically $ newTVar HashMap.empty
  withAsync (cursorConsumer cursorsMap) $ \ asyncCursorConsumer -> do
    link asyncCursorConsumer
    Timer.withAsyncTimer timerConf $ cursorCommitter cursorsMap nMaxEvents

  where -- The cursorsConsumer drains the cursors queue and adds each
        -- cursor to the provided cursorsMap.
        cursorConsumer cursorsMap = loop
          where loop = do
                  _cursor <- liftIO . atomically $ do
                    (nEvents, cursor) <- readTBQueue queue
                    modifyTVar cursorsMap (HashMap.insertWith updateCursor (cursor^.L.partition) (nEvents, cursor))
                    pure cursor
                  loop

        updateCursor cursorNew (nEventsOld, _) =
          cursorNew & _1 %~ (+ nEventsOld)

        cursorCommitter cursorsMap nMaxEvents timer = loop
          where loop = do
                  race (Timer.wait timer) (maxEventsReached cursorsMap nMaxEvents) >>= \ case
                    Left _ -> commitAllCursors eventStream cursorsMap
                    Right cursors -> do
                      Timer.reset timer
                      forM_ cursors (commitOneCursor eventStream)
                  loop

        maxEventsReached
          :: MonadIO m
          => TVar (HashMap PartitionName (Int, SubscriptionCursor))
          -> Int
          -> m [SubscriptionCursor]
        maxEventsReached cursorsMap nMaxEvents = liftIO . atomically $ do
          cursorsList <- HashMap.toList <$> readTVar cursorsMap
          let (cursorsCommit, cursorsNotCommit) = partition (shouldBeCommitted nMaxEvents) cursorsList
          if null cursorsCommit
            then retry
            else do writeTVar cursorsMap (HashMap.fromList cursorsNotCommit)
                    pure $ map (view (_2._2)) cursorsCommit

        shouldBeCommitted nMaxEvents cursor = cursor^._2._1  > nMaxEvents

-- | This function commits all cursors in the provided cursorsMap.
commitAllCursors
  :: (MonadNakadi b m, MonadIO m)
  => SubscriptionEventStream
  -> TVar (HashMap PartitionName (Int, SubscriptionCursor))
  -> m ()
commitAllCursors eventStream cursorsMap = do
  cursors <- liftIO . atomically $ swapTVar cursorsMap HashMap.empty
  forM_ cursors $ \ (_nEvents, cursor) -> commitOneCursor eventStream cursor


-- | This function takes care of committing a single cursor. Exceptions will be
-- catched and logged, but the failure will NOT be propagated. This means that
-- Nakadi itself is in control of disconnecting us.
commitOneCursor
  :: (MonadIO m, MonadNakadi b m)
  => SubscriptionEventStream -> SubscriptionCursor -> m ()
commitOneCursor eventStream cursor = do
  config <- nakadiAsk
  catchAny (subscriptionCursorCommit eventStream [cursor]) $ \ exn -> nakadiLiftBase $
    case config^.L.logFunc of
      Just logFunc -> logFunc "nakadi-client" LevelWarn $ toLogStr $
        "Failed to commit cursor " <> tshow cursor <> ": " <> tshow exn
      Nothing ->
        pure ()
