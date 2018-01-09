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

import qualified Data.ByteString.Lazy                 as LB (ByteString)
import           Network.Nakadi.Internal.Types.Logger

-- | Config

type StreamConnectCallback m = Maybe (LogFunc m) -> Response () -> m ()

-- | Type synonym for user-provided callbacks which are used for HTTP
-- Errror propagation.
type HttpErrorCallback m = Request -> HttpException -> RetryStatus -> Bool -> m ()

type ConfigIO = Config IO

data Config m = Config
  { _requestTemplate                :: Request
  , _requestModifier                :: Request -> m Request
  , _manager                        :: Manager
  , _consumeParameters              :: ConsumeParameters
  , _deserializationFailureCallback :: Maybe (ByteString -> Text -> m ())
  , _streamConnectCallback          :: Maybe (StreamConnectCallback m)
  , _logFunc                        :: Maybe (LogFunc m)
  , _retryPolicy                    :: RetryPolicyM m
  , _http                           :: HttpBackend
  , _httpErrorCallback              :: Maybe (HttpErrorCallback m)
  }

-- | Type encapsulating the HTTP backend functions used by this
-- package. By default the corresponding functions from the
-- http-client package are used. Useful, for e.g., testing.

data HttpBackend = HttpBackend
  { _httpLbs       :: Request -> IO (Response LB.ByteString)
  , _responseOpen  :: Request -> IO (Response BodyReader)
  , _responseClose :: Response BodyReader -> IO ()
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
