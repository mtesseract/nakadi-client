{-|
Module      : Network.Nakadi.Internal.Committer.NoBuffer
Description : Implementation unbuffered Cursor Committing Strategy
Copyright   : (c) Moritz Clasmeier 2018
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Network.Nakadi.Internal.Committer.NoBuffer where

import           Network.Nakadi.Internal.Prelude

import           Control.Lens
import           Control.Monad.Logger
import qualified Network.Nakadi.Internal.Lenses       as L
import           Network.Nakadi.Internal.Types
import           Network.Nakadi.Subscriptions.Cursors

import           UnliftIO.STM

-- | Implementation for the 'CommitNoBuffer' strategy: We simply read
-- every cursor and commit it in order.
committerNoBuffer
  :: ( MonadNakadi b m
     , MonadUnliftIO m
     , MonadMask m )
  => SubscriptionEventStream
  -> TBQueue (Int, SubscriptionCursor)
  -> m ()
committerNoBuffer eventStream queue = loop
  where loop = do
          config <- nakadiAsk
          (_nEvents, cursor) <- liftIO . atomically . readTBQueue $ queue
          catchAny (subscriptionCursorCommit eventStream [cursor]) $ \ exn -> do
            nakadiLiftBase $
              case config^.L.logFunc of
                Just logFunc -> logFunc "nakadi-client" LevelWarn $ toLogStr $
                  "Failed to commit cursor " <> tshow cursor <> ": " <> tshow exn
                Nothing ->
                  pure ()
          loop
