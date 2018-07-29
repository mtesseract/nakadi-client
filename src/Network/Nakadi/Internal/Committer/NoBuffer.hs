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

import           Network.Nakadi.Internal.Committer.Shared
import           Network.Nakadi.Internal.Types

import           UnliftIO.STM

-- | Implementation for the 'CommitNoBuffer' strategy: We simply read
-- every cursor and commit it in order.
committerNoBuffer
  :: (MonadNakadi b m, MonadUnliftIO m, MonadMask m)
  => SubscriptionEventStream
  -> TBQueue (Int, SubscriptionCursor)
  -> m ()
committerNoBuffer = unbufferedCommitLoop
