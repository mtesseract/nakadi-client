{-|
Module      : Network.Nakadi.Config
Description : Nakadi Client Configuration
Copyright   : (c) Moritz Clasmeier 2017, 2018
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX

This module implements support for creating and manipulating Nakadi
client configurations.
-}

module Network.Nakadi.Config
  ( newConfig
  , newConfigIO
  , newConfigWithDedicatedManager
  , newConfigFromEnv
  , setHttpManager
  , setRequestModifier
  , setDeserializationFailureCallback
  , setStreamConnectCallback
  , setHttpErrorCallback
  , setLogFunc
  , setRetryPolicy
  , setWorkerThreads
  , setMaxUncommittedEvents
  , setBatchLimit
  , setStreamLimit
  , setBatchFlushTimeout
  , setStreamTimeout
  , setStreamKeepAliveLimit
  , setFlowId
  , setCommitStrategy
  , setShowTimeLag
  )
where

import           Network.Nakadi.Internal.Prelude

import           Control.Lens
import           Control.Retry
import           Network.HTTP.Client            ( Manager
                                                , ManagerSettings
                                                )
import           Network.HTTP.Client.TLS        ( newTlsManagerWith )
import           Network.Nakadi.Internal.HttpBackendIO
import qualified Network.Nakadi.Internal.Lenses
                                               as L
import           Network.Nakadi.Internal.Types
import           Network.Nakadi.Internal.Config

-- | Producs a new configuration, with mandatory HTTP manager, default
-- consumption parameters and HTTP request template.
newConfigIO
  :: Request           -- ^ Request Template
  -> ConfigIO          -- ^ Resulting Configuration
newConfigIO = newConfig httpBackendIO

-- | Produce a new configuration, with optional HTTP manager settings
-- and mandatory HTTP request template.
newConfigWithDedicatedManager
  :: (MonadIO b, MonadMask b, MonadIO m)
  => ManagerSettings -- ^ Optional 'ManagerSettings'
  -> Request         -- ^ Request template for Nakadi requests
  -> m (Config b)    -- ^ Resulting Configuration
newConfigWithDedicatedManager mngrSettings request = do
  manager <- newTlsManagerWith mngrSettings
  pure $ newConfig httpBackendIO request & setHttpManager manager

-- | Install an HTTP Manager in the provided configuration. If not
-- set, HTTP requests will use a global default manager.
setHttpManager :: Manager -> Config m -> Config m
setHttpManager mngr = L.manager .~ Just mngr

-- | Install a request modifier in the provided configuration. This
-- can be used for e.g. including access tokens in HTTP requests to
-- Nakadi.
setRequestModifier :: (Request -> m Request) -> Config m -> Config m
setRequestModifier = (L.requestModifier .~)

-- | Install a callback in the provided configuration to use in case
-- of deserialization failures when consuming events.
setDeserializationFailureCallback :: (ByteString -> Text -> m ()) -> Config m -> Config m
setDeserializationFailureCallback cb = L.deserializationFailureCallback .~ Just cb

-- | Install a callback in the provided configuration which is used
-- after having successfully established a streaming Nakadi
-- connection.
setStreamConnectCallback :: StreamConnectCallback m -> Config m -> Config m
setStreamConnectCallback cb = L.streamConnectCallback .~ Just cb

-- | Install a callback in the provided configuration which is called
-- on HTTP 5xx errors. This allows the user to act on such error
-- conditions by e.g. logging errors or updating metrics. Note that
-- this callback is called synchronously, thus blocking in this
-- callback delays potential retry attempts.
setHttpErrorCallback :: HttpErrorCallback m -> Config m -> Config m
setHttpErrorCallback cb = L.httpErrorCallback .~ Just cb

-- | Install a logger callback in the provided configuration.
setLogFunc :: LogFunc m -> Config m -> Config m
setLogFunc logFunc = L.logFunc .~ Just logFunc

-- | Set a custom retry policy in the provided configuration.
setRetryPolicy :: RetryPolicyM IO -> Config m -> Config m
setRetryPolicy = (L.retryPolicy .~)

-- | Set flow ID in the provided configuration.
setFlowId :: FlowId -> Config m -> Config m
setFlowId flowId = L.flowId .~ Just flowId

-- | Set flow ID in the provided configuration.
setCommitStrategy :: CommitStrategy -> Config m -> Config m
setCommitStrategy = (L.commitStrategy .~)

-- | Set number of worker threads that should be spawned on
-- subscription consumption. The (per event-type) partitions of the
-- subscription to be consumed will then be mapped onto this finite
-- set of workers.
setWorkerThreads :: Int -> Config m -> Config m
setWorkerThreads n = (L.worker . L.nThreads .~ n)

-- | Set maximum number of uncommitted events.
setMaxUncommittedEvents :: Int32 -> Config m -> Config m
setMaxUncommittedEvents = (L.maxUncommittedEvents ?~)

-- | Set batch limit.
setBatchLimit :: Int32 -> Config m -> Config m
setBatchLimit = (L.batchLimit ?~)

-- | Set stream limit.
setStreamLimit :: Int32 -> Config m -> Config m
setStreamLimit = (L.streamLimit ?~)

-- | Set batch-flush-timeout limit.
setBatchFlushTimeout :: Int32 -> Config m -> Config m
setBatchFlushTimeout = (L.batchFlushTimeout ?~)

-- | Set stream timeout.
setStreamTimeout :: Int32 -> Config m -> Config m
setStreamTimeout = (L.streamTimeout ?~)

-- | Set stream-keep-alive-limit.
setStreamKeepAliveLimit :: Int32 -> Config m -> Config m
setStreamKeepAliveLimit = (L.streamKeepAliveLimit ?~)

setShowTimeLag :: Bool -> Config m -> Config m
setShowTimeLag flag = L.subscriptionStats ?~ SubscriptionStatsConf {_showTimeLag = flag}
