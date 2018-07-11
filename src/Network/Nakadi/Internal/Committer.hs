{-|
Module      : Network.Nakadi.Internal.Committer
Description : Implementation of Cursor Committing Strategies
Copyright   : (c) Moritz Clasmeier 2017, 2018
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX

This internal module implements cursor committing strategies to be
used by the subscription API.
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Network.Nakadi.Internal.Committer where

import           Network.Nakadi.Internal.Prelude

import           Conduit
import           Control.Lens
import           Control.Monad.Logger
import           UnliftIO.STM

import qualified Network.Nakadi.Internal.Lenses
                                               as L
import           Network.Nakadi.Internal.Types

import           Network.Nakadi.Internal.Committer.NoBuffer
import           Network.Nakadi.Internal.Committer.Shared
import           Network.Nakadi.Internal.Committer.SmartBuffer
import           Network.Nakadi.Internal.Committer.TimeBuffer

-- | Main function for the cursor committer thread. Logic depends on
-- the provided buffering strategy.
subscriptionCommitter
  :: forall b m
   . (MonadNakadi b m, MonadUnliftIO m, MonadMask m)
  => CommitBufferingStrategy
  -> SubscriptionEventStream
  -> TBQueue (Int, SubscriptionCursor)
  -> m ()
subscriptionCommitter CommitNoBuffer eventStream queue = committerNoBuffer eventStream queue
subscriptionCommitter (CommitTimeBuffer millis) eventStream queue =
  committerTimeBuffer millis eventStream queue
subscriptionCommitter CommitSmartBuffer eventStream queue = committerSmartBuffer eventStream queue

-- | Sink which can be used as sink for Conduits processing
-- subscriptions events. This sink takes care of committing events. It
-- can consume any values which contain Subscription Cursors.
subscriptionSink
  :: (MonadIO m, MonadNakadi b m)
  => SubscriptionEventStream
  -> ConduitM (SubscriptionEventStreamBatch a) void m ()
subscriptionSink eventStream = do
  config <- lift nakadiAsk
  awaitForever $ \batch -> lift $ do
    let cursor = batch ^. L.cursor
    catchAny (commitOneCursor eventStream cursor) $ \exn ->
      nakadiLiftBase $ case config ^. L.logFunc of
        Just logFunc ->
          logFunc "nakadi-client" LevelWarn
            $  toLogStr
            $  "Failed to synchronously commit cursor: "
            <> tshow exn
        Nothing -> pure ()
