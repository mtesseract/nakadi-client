module Nakadi.Config where

import           Control.Lens
import qualified Nakadi.Internal.Lenses  as L
import           Nakadi.Internal.Prelude
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS

import           Nakadi.Types

-- | Config

newNakadiConfig' :: (MonadIO m, MonadThrow m)
                 => Manager
                 -> ConsumeParameters
                 -> Request
                 -> m Config
newNakadiConfig' manager consumeParameters request =
  return Config { _consumeParameters              = consumeParameters
                , _manager                        = manager
                , _requestTemplate                = request
                , _requestModifier                = return
                , _deserializationFailureCallback = Nothing }

newNakadiConfig :: (MonadIO m, MonadThrow m)
                => Maybe ManagerSettings
                -> Request
                -> m Config
newNakadiConfig mngrSettings request = do
  manager <- newTlsManagerWith (fromMaybe tlsManagerSettings mngrSettings)
  newNakadiConfig' manager defaultConsumeParameters request

setRequestModifier :: (Request -> IO Request) -> Config -> Config
setRequestModifier = (L.requestModifier .~)

setDeserializationFailureCallback :: (ByteString -> IO ()) -> Config -> Config
setDeserializationFailureCallback callback = L.deserializationFailureCallback .~ Just callback

-- | ConsumeParameters

defaultConsumeParameters :: ConsumeParameters
defaultConsumeParameters = ConsumeParameters
  { _maxUncommittedEvents = Nothing
  , _batchLimit           = Nothing
  , _streamLimit          = Nothing
  , _batchFlushTimeout    = Nothing
  , _streamTimeout        = Nothing
  , _streamKeepAliveLimit = Nothing
  , _flowId               = Nothing
  }

-- Setters

setMaxUncommittedEvents :: Int32 -> ConsumeParameters -> ConsumeParameters
setMaxUncommittedEvents n params = params & L.maxUncommittedEvents .~ Just n

setBatchLimit :: Int32 -> ConsumeParameters -> ConsumeParameters
setBatchLimit n params = params & L.batchLimit .~ Just n

setStreamLimit :: Int32 -> ConsumeParameters -> ConsumeParameters
setStreamLimit n params = params & L.streamLimit .~ Just n

setBatchFlushTimeout :: Int32 -> ConsumeParameters -> ConsumeParameters
setBatchFlushTimeout n params = params & L.batchFlushTimeout .~ Just n

setStreamTimeout :: Int32 -> ConsumeParameters -> ConsumeParameters
setStreamTimeout n params = params & L.streamTimeout .~ Just n

setStreamKeepAliveLimit :: Int32 -> ConsumeParameters -> ConsumeParameters
setStreamKeepAliveLimit n params = params & L.streamKeepAliveLimit .~ Just n

setFlowId :: Text -> ConsumeParameters -> ConsumeParameters
setFlowId flowId = L.flowId .~ Just flowId
