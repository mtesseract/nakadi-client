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
import           Control.Concurrent.STM               (TBQueue, atomically,
                                                       modifyTVar, newTBQueue,
                                                       newTVar, readTBQueue,
                                                       swapTVar, writeTBQueue)
import           Control.Lens
import           Control.Monad.IO.Unlift
import           Control.Monad.Logger
import           Data.Aeson
import qualified Data.Conduit.List                    as Conduit (map, mapM_)
import qualified Data.HashMap.Strict                  as HashMap
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
     , FromJSON a
     , L.HasNakadiSubscriptionCursor c )
  => Maybe ConsumeParameters
  -> SubscriptionId
  -> ConduitM (SubscriptionEventStreamBatch a) c m ()
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
     , FromJSON a
     , L.HasNakadiSubscriptionCursor c )
  => Config b
  -> SubscriptionId
  -> ConduitM (SubscriptionEventStreamBatch a) c m ()
  -> Response (ConduitM () ByteString m ())
  -> m ()
subscriptionProcessHandler config subscriptionId processor response = do
  eventStream <- buildSubscriptionEventStream subscriptionId response
  let producer = responseBody response
                 .| linesUnboundedAsciiC
                 .| conduitDecode config
                 .| processor
                 .| Conduit.map (view L.subscriptionCursor)
  case config^.L.commitStrategy of
    CommitSync ->
      runConduit $ producer .| subscriptionSink eventStream
    CommitAsync bufferingStrategy -> do
      queue <- liftIO . atomically $ newTBQueue 1024
      withAsync (subscriptionCommitter bufferingStrategy eventStream queue) $
        \ asyncHandle -> do
          link asyncHandle
          runConduit $
            producer
            .| Conduit.mapM_ (liftIO . atomically . writeTBQueue queue)

-- | Sink which can be used as sink for Conduits processing
-- subscriptions events. This sink takes care of committing events. It
-- can consume any values which contain Subscription Cursors.
subscriptionSink
  :: (MonadIO m, MonadNakadi b m)
  => SubscriptionEventStream
  -> ConduitM SubscriptionCursor void m ()
subscriptionSink eventStream = do
  config <- lift nakadiAsk
  awaitForever $ \ cursor -> lift $ do
    liftIO . putStrLn $ "Committing cursor: " <> show cursor
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
  -> TBQueue SubscriptionCursor
  -> m ()

subscriptionCommitter CommitNoBuffer eventStream queue = loop
  where loop = do
          config <- nakadiAsk
          cursor <- liftIO . atomically . readTBQueue $ queue
          liftIO . putStrLn $ "Committing cursor: " <> show cursor
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
                    cursor <- readTBQueue queue
                    modifyTVar cursorsMap (HashMap.insert (cursor^.L.partition) cursor)
                    pure cursor
                  loop

        -- The cursorsCommitter is responsible for periodically committing
        -- the cursors in the provided cursorsMap.
        cursorCommitter cursorsMap timer = loop
          where loop = do
                  Timer.wait timer
                  commitAllCursors cursorsMap
                  loop

        -- This function commits all cursors in the provided cursorsMap.
        commitAllCursors cursorsMap = do
          cursors <- liftIO . atomically $ swapTVar cursorsMap HashMap.empty
          forM_ cursors commitOneCursor

        -- This function takes care of committing a single cursor. Exceptions will be
        -- catched and logged, but the failure will NOT be propagated. This means that
        -- Nakadi itself is in control of disconnecting us.
        commitOneCursor cursor = do
          config <- nakadiAsk
          catchAny (subscriptionCursorCommit eventStream [cursor]) $ \ exn -> nakadiLiftBase $
            case config^.L.logFunc of
              Just logFunc -> logFunc "nakadi-client" LevelWarn $ toLogStr $
                "Failed to commit cursor " <> tshow cursor <> ": " <> tshow exn
              Nothing ->
                pure ()
