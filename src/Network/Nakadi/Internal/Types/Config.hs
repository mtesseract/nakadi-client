{-|
Module      : Network.Nakadi.Internal.Types.Config
Description : Nakadi Client Configuration Types (Internal)
Copyright   : (c) Moritz Clasmeier 2017, 2018
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

import           Conduit
import           Control.Retry
import qualified Data.ByteString.Lazy          as LB
import           Network.HTTP.Client
import           Network.Nakadi.Internal.Prelude
import           Network.Nakadi.Internal.Types.Logger
import           Network.Nakadi.Internal.Types.Service
import           Network.Nakadi.Internal.Types.Subscriptions

-- | Config

type StreamConnectCallback m = Response () -> m ()

-- | Type synonym for user-provided callbacks which are used for HTTP
-- Errror propagation.
type HttpErrorCallback m = Request -> HttpException -> RetryStatus -> Bool -> m ()

type ConfigIO = Config IO

data Config m =
  Config { _requestTemplate                :: Request
         , _requestModifier                :: Request -> m Request
         , _manager                        :: Maybe Manager
         , _deserializationFailureCallback :: Maybe (ByteString -> Text -> m ())
         , _streamConnectCallback          :: Maybe (StreamConnectCallback m)
         , _logFunc                        :: Maybe (LogFunc m)
         , _retryPolicy                    :: RetryPolicyM IO
         , _http                           :: HttpBackend m
         , _httpErrorCallback              :: Maybe (HttpErrorCallback m)
         , _flowId                         :: Maybe FlowId
         , _commitStrategy                 :: CommitStrategy
         , _subscriptionStats              :: Maybe SubscriptionStatsConf
         , _maxUncommittedEvents           :: Maybe Int32
         , _batchLimit                     :: Maybe Int32
         , _streamLimit                    :: Maybe Int32
         , _batchFlushTimeout              :: Maybe Int32
         , _streamTimeout                  :: Maybe Int32
         , _streamKeepAliveLimit           :: Maybe Int32
         , _worker                         :: WorkerConfig
         }

data HttpBackend b = HttpBackend
  { _httpLbs           :: Config b -> Request -> Maybe Manager -> b (Response LB.ByteString)
  , _httpResponseOpen  :: Config b -> Request -> Maybe Manager -> b (Response (ConduitM () ByteString b ()))
  , _httpResponseClose :: Response () -> b ()
  }

{-# ANN WorkerConfig ("HLint: ignore Use newtype instead of data" :: String) #-}
data WorkerConfig = WorkerConfig
  { _nThreads      :: Int
  }

{-# ANN SubscriptionStatsConf ("HLint: ignore Use newtype instead of data" :: String) #-}
data SubscriptionStatsConf = SubscriptionStatsConf
  { _showTimeLag :: Bool
  }
