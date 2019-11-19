{-# LANGUAGE FlexibleContexts    #-}

module Network.Nakadi.Examples.Subscription.Process
  ( dumpSubscription
  )
where

import           ClassyPrelude
import           Data.Aeson
import           Data.Void
import qualified Network.Nakadi                as Nakadi
import           Network.Nakadi                 ( MonadNakadi )
import           Control.Monad.Logger
import           Control.Monad.Catch            ( MonadMask )
import           Control.Monad.Trans.Resource

dumpSubscription
  :: (MonadLogger m, MonadNakadi IO m, MonadUnliftIO m, MonadMask m)
  => Nakadi.SubscriptionId
  -> m Void
dumpSubscription subscriptionId = runResourceT
  $ Nakadi.subscriptionProcess subscriptionId processBatch
 where
  processBatch :: MonadLogger m => Nakadi.SubscriptionEventStreamBatch Value -> m ()
  processBatch batch = logInfoN (tshow batch)
