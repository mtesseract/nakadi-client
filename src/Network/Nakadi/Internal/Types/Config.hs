{-|
Module      : Network.Nakadi.Internal.Types.Config
Description : Nakadi Client Configuration Types (Internal)
Copyright   : (c) Moritz Schulte 2017
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX

Internal configuration specific types.
-}

{-# LANGUAGE StrictData #-}

module Network.Nakadi.Internal.Types.Config where

import           Network.Nakadi.Internal.Prelude

import           Control.Retry
import           Network.HTTP.Client

import qualified Data.ByteString.Lazy as LB (ByteString)
import           Network.Nakadi.Types.Logger

-- | Config

type StreamConnectCallback = Maybe LogFunc -> Response () -> IO ()

data Config = Config
  { _requestTemplate                :: Request
  , _requestModifier                :: Request -> IO Request
  , _manager                        :: Manager
  , _consumeParameters              :: ConsumeParameters
  , _deserializationFailureCallback :: Maybe (ByteString -> Text -> IO ())
  , _streamConnectCallback          :: Maybe StreamConnectCallback
  , _logFunc                        :: Maybe LogFunc
  , _retryPolicy                    :: RetryPolicyM IO
  , _http                           :: HttpBackend
  }

data HttpBackend = HttpBackend
  { _httpLbs                        :: Request -> IO (Response LB.ByteString)
  , _responseOpen                   :: Request -> Manager -> IO (Response BodyReader)
  , _responseClose                  :: Response BodyReader -> IO ()
  }

-- | ConsumeParameters

data ConsumeParameters = ConsumeParameters
  { _maxUncommittedEvents :: Maybe Int32
  , _batchLimit           :: Maybe Int32
  , _streamLimit          :: Maybe Int32
  , _batchFlushTimeout    :: Maybe Int32
  , _streamTimeout        :: Maybe Int32
  , _streamKeepAliveLimit :: Maybe Int32
  , _flowId               :: Maybe Text
  } deriving (Show, Eq, Ord)
