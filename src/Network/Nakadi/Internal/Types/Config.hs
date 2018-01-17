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

{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Network.Nakadi.Internal.Types.Config where

import           Control.Retry
import           Network.HTTP.Client
import           Network.Nakadi.Internal.Prelude
import           Network.Nakadi.Internal.Types.Logger

-- | Config

type StreamConnectCallback m = Response () -> m ()

-- | Type synonym for user-provided callbacks which are used for HTTP
-- Errror propagation.
type HttpErrorCallback m = Request -> HttpException -> RetryStatus -> Bool -> m ()

type ConfigIO = Config IO

data Config m where
  Config :: { _requestTemplate                :: Request
            , _requestModifier                :: Request -> m Request
            , _manager                        :: Manager
            , _consumeParameters              :: ConsumeParameters
            , _deserializationFailureCallback :: Maybe (ByteString -> Text -> m ())
            , _streamConnectCallback          :: Maybe (StreamConnectCallback m)
            , _logFunc                        :: Maybe (LogFunc m)
            , _retryPolicy                    :: RetryPolicyM m
            , _httpErrorCallback              :: Maybe (HttpErrorCallback m)
            } -> Config m

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
