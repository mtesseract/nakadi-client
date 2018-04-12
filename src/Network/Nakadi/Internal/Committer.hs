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
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Network.Nakadi.Internal.Committer where

import           Network.Nakadi.Internal.Prelude

import           Conduit
import           Control.Lens
import           UnliftIO.STM

import qualified Network.Nakadi.Internal.Lenses                as L
import           Network.Nakadi.Internal.Logging
import           Network.Nakadi.Internal.Types

import           Network.Nakadi.Internal.Committer.NoBuffer
import           Network.Nakadi.Internal.Committer.Shared
import           Network.Nakadi.Internal.Committer.SmartBuffer
import           Network.Nakadi.Internal.Committer.TimeBuffer

-- | Main function for the cursor committer thread. Logic depends on
-- the provided buffering strategy.
subscriptionCommitter
  :: forall b m
   . ( MonadNakadi b m
     , MonadUnliftIO m
     , MonadMask m )
  => CommitBufferingStrategy
  -> ConsumeParameters
  -> SubscriptionEventStream
  -> TBQueue (Int, SubscriptionCursor)
  -> m ()
subscriptionCommitter CommitNoBuffer _consumeParams eventStream queue =
  committerNoBuffer eventStream queue
subscriptionCommitter (CommitTimeBuffer millis) _consumeParams eventStream queue =
  committerTimeBuffer millis eventStream queue
subscriptionCommitter CommitSmartBuffer consumeParams eventStream queue =
  committerSmartBuffer consumeParams eventStream queue

-- | Sink which can be used as sink for Conduits processing
-- subscriptions events. This sink takes care of committing events. It
-- can consume any values which contain Subscription Cursors.
subscriptionSink
  :: (MonadIO m, MonadNakadi b m)
  => SubscriptionEventStream
  -> ConduitM (SubscriptionEventStreamBatch a) void m ()
subscriptionSink eventStream =
  awaitForever $ \ batch -> lift $
    let cursor = batch^.L.cursor
    in catchAny (commitOneCursor eventStream cursor) $ \ exn ->
      nakadiLogWarn [fmt|Failed to commit cursor ${tshow cursor}: $exn|]
