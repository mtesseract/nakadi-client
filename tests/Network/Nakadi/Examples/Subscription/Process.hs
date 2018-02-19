{-# LANGUAGE FlexibleContexts    #-}

module Network.Nakadi.Examples.Subscription.Process (dumpSubscription) where

import           ClassyPrelude
import           Data.Aeson
import qualified Network.Nakadi as Nakadi
import Network.Nakadi (MonadNakadi)
import Control.Monad.Logger
import Control.Monad.Catch (MonadMask)

dumpSubscription :: (MonadLogger m, MonadNakadi IO m, MonadMask m) => Nakadi.SubscriptionId -> m ()
dumpSubscription subscriptionId =
  Nakadi.subscriptionProcess Nothing subscriptionId processBatch

  where processBatch :: MonadLogger m => Nakadi.SubscriptionEventStreamBatch Value -> m ()
        processBatch batch =
          logInfoN (tshow batch)
