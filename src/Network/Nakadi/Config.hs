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
  , setHttpManager
  , setRequestModifier
  , setDeserializationFailureCallback
  , setStreamConnectCallback
  , setHttpErrorCallback
  , setLogFunc
  , setRetryPolicy
  , setMaxUncommittedEvents
  , setBatchLimit
  , setStreamLimit
  , setBatchFlushTimeout
  , setStreamTimeout
  , setStreamKeepAliveLimit
  , setFlowId
  , defaultConsumeParameters
  ) where

import           Network.Nakadi.Internal.Prelude

import           Control.Lens
import           Control.Retry
import           Network.HTTP.Client                   (Manager,
                                                        ManagerSettings)
import           Network.HTTP.Client.TLS               (newTlsManagerWith)
import           Network.Nakadi.Internal.Config
import           Network.Nakadi.Internal.HttpBackendIO
import qualified Network.Nakadi.Internal.Lenses        as L
import           Network.Nakadi.Internal.Types

-- | Default retry policy.
defaultRetryPolicy :: MonadIO m => RetryPolicyM m
defaultRetryPolicy = fullJitterBackoff 2 <> limitRetries 5

-- | Produces a new configuration, with mandatory HTTP manager, default
-- consumption parameters and HTTP request template.
newConfig
  :: Monad b
  => HttpBackend b
  -> Request           -- ^ Request Template
  -> Config b          -- ^ Resulting Configuration
newConfig httpBackend request =
  Config { _consumeParameters              = Nothing
         , _manager                        = Nothing
         , _requestTemplate                = request
         , _requestModifier                = pure
         , _deserializationFailureCallback = Nothing
         , _streamConnectCallback          = Nothing
         , _logFunc                        = Nothing
         , _retryPolicy                    = defaultRetryPolicy
         , _http                           = httpBackend
         , _httpErrorCallback              = Nothing
         , _flowId                         = Nothing
         }

-- | Produces a new configuration, specialized to @IO@.
newConfigIO
  :: Request           -- ^ Request Template
  -> ConfigIO          -- ^ Resulting Configuration
newConfigIO = newConfig httpBackendIO

-- | Produces a new configuration, with optional HTTP manager settings
-- and mandatory HTTP request template. The configuration contains a reference
-- to a dedicated HTTP Manager that is used for all HTTP requests.
newConfigWithDedicatedManager ::
  (MonadIO b, MonadMask b, MonadIO m)
  => ManagerSettings -- ^ Optional 'ManagerSettings'
  -> Request         -- ^ Request template for Nakadi requests
  -> m (Config b)    -- ^ Resulting Configuration
newConfigWithDedicatedManager mngrSettings request = do
  manager <- newTlsManagerWith mngrSettings
  pure $ newConfig httpBackendIO request & setHttpManager manager

-- | Install an HTTP Manager in the provided configuration. If not
-- set, HTTP requests will use a global default manager.
setHttpManager
  :: Manager
  -> Config m
  -> Config m
setHttpManager mngr = L.manager .~ Just mngr

-- | Install a request modifier in the provided configuration. This
-- can be used for e.g. including access tokens in HTTP requests to
-- Nakadi.
setRequestModifier ::
  (Request -> m Request)
  -> Config m
  -> Config m
setRequestModifier = (L.requestModifier .~)

-- | Install a callback in the provided configuration to use in case
-- of deserialization failures when consuming events.
setDeserializationFailureCallback ::
  (ByteString -> Text -> m ())
  -> Config m
  -> Config m
setDeserializationFailureCallback cb = L.deserializationFailureCallback .~ Just cb

-- | Install a callback in the provided configuration which is used
-- after having successfully established a streaming Nakadi
-- connection.
setStreamConnectCallback ::
  StreamConnectCallback m
  -> Config m
  -> Config m
setStreamConnectCallback cb = L.streamConnectCallback .~ Just cb

-- | Install a callback in the provided configuration which is called
-- on HTTP 5xx errors. This allows the user to act on such error
-- conditions by e.g. logging errors or updating metrics. Note that
-- this callback is called synchronously, thus blocking in this
-- callback delays potential retry attempts.
setHttpErrorCallback ::
  HttpErrorCallback m
  -> Config m
  -> Config m
setHttpErrorCallback cb = L.httpErrorCallback .~ Just cb

-- | Install a logger callback in the provided configuration.
setLogFunc ::
  LogFunc m
  -> Config m
  -> Config m
setLogFunc logFunc = L.logFunc .~ Just logFunc

-- | Set a custom retry policy in the provided configuration.
setRetryPolicy ::
  RetryPolicyM IO
  -> Config m
  -> Config m
setRetryPolicy = (L.retryPolicy .~)

-- | Set flow ID in the provided configuration.
setFlowId
  :: FlowId
  -> Config m
  -> Config m
setFlowId flowId = L.flowId .~ Just flowId

-- | Set maximum number of uncommitted events in the provided value of
-- consumption parameters.
setMaxUncommittedEvents :: Int32 -> ConsumeParameters -> ConsumeParameters
setMaxUncommittedEvents n params = params & L.maxUncommittedEvents .~ Just n

-- | Set batch limit in the provided value of consumption parameters.
setBatchLimit :: Int32 -> ConsumeParameters -> ConsumeParameters
setBatchLimit n params = params & L.batchLimit .~ Just n

-- | Set stream limit in the provided value of consumption parameters.
setStreamLimit :: Int32 -> ConsumeParameters -> ConsumeParameters
setStreamLimit n params = params & L.streamLimit .~ Just n

-- | Set batch-flush-timeout limit in the provided value of
-- consumption parameters.
setBatchFlushTimeout :: Int32 -> ConsumeParameters -> ConsumeParameters
setBatchFlushTimeout n params = params & L.batchFlushTimeout .~ Just n

-- | Set stream timeout in the provided value of consumption parameters.
setStreamTimeout :: Int32 -> ConsumeParameters -> ConsumeParameters
setStreamTimeout n params = params & L.streamTimeout .~ Just n

-- | Set stream-keep-alive-limit in the provided value of consumption
-- parameters.
setStreamKeepAliveLimit :: Int32 -> ConsumeParameters -> ConsumeParameters
setStreamKeepAliveLimit n params = params & L.streamKeepAliveLimit .~ Just n
