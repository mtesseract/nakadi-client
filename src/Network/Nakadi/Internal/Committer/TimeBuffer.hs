{-|
Module      : Network.Nakadi.Internal.Committer.TimeBuffer
Description : Implementation of TimeBuffer based Cursor Committing Strategy
Copyright   : (c) Moritz Clasmeier 2018
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Network.Nakadi.Internal.Committer.TimeBuffer where

import           Network.Nakadi.Internal.Prelude

import qualified Control.Concurrent.Async.Timer
                                               as Timer
import qualified Data.HashMap.Strict           as HashMap

import           Data.Function                  ( (&) )
import           Network.Nakadi.Internal.Committer.Shared
import           Network.Nakadi.Internal.Types

import           UnliftIO.Async
import           UnliftIO.STM

-- | Implementation of the 'CommitTimeBuffer' strategy: We use an
-- async timer for committing cursors at specified intervals.
--
-- The 'StagedCursor's in the 'CommitTimeBuffer' case carry no
-- additional information, just the subscription cursors.
committerTimeBuffer
  :: (MonadNakadi b m, MonadUnliftIO m, MonadMask m)
  => Int32
  -> SubscriptionEventStream
  -> TBQueue (Int, SubscriptionCursor)
  -> m ()
committerTimeBuffer millis eventStream queue = do
  let timerConf = Timer.defaultConf & Timer.setInitDelay (fromIntegral millis) & Timer.setInterval
        (fromIntegral millis)
  cursorsMap <- liftIO $ newTVarIO (HashMap.empty :: StagedCursorsMap SubscriptionCursor)
  withAsync (cursorConsumer cursorsMap) $ \asyncCursorConsumer -> do
    link asyncCursorConsumer
    Timer.withAsyncTimer timerConf $ \timer -> forever $ do
      Timer.wait timer
      commitAllCursors identity eventStream cursorsMap
 where -- The cursorsConsumer drains the cursors queue and adds each
       -- cursor to the provided cursorsMap.
   cursorConsumer cursorsMap = forever . liftIO . atomically $ do
     (_, cursor) <- readTBQueue queue
     modifyTVar cursorsMap (HashMap.insert (cursorKey cursor) cursor)
