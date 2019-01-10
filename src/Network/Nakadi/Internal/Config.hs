{-|
Module      : Network.Nakadi.Internal.Config
Description : Nakadi Client Configuration (Internal)
Copyright   : (c) Moritz Clasmeier 2017, 2018, 2019
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX

Internal module containing configuration specific code.
-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}

module Network.Nakadi.Internal.Config where

import           System.Environment

import           Control.Retry
import           Network.HTTP.Client            ( parseRequest )

import           Network.Nakadi.Internal.Prelude
import           Network.Nakadi.Internal.HttpBackendIO
import           Network.Nakadi.Internal.Types

buildConsumeQueryParameters :: Config m -> [(ByteString, ByteString)]
buildConsumeQueryParameters Config {..} = catMaybes
  [ ("batch_limit", ) . encodeUtf8 . tshow <$> _batchLimit
  , ("stream_limit", ) . encodeUtf8 . tshow <$> _streamLimit
  , ("batch_flush_timeout", ) . encodeUtf8 . tshow <$> _batchFlushTimeout
  , ("stream_timeout", ) . encodeUtf8 . tshow <$> _streamTimeout
  , ("max_uncommitted_events", ) . encodeUtf8 . tshow <$> _maxUncommittedEvents
  , ("stream_keep_alive_limit", ) . encodeUtf8 . tshow <$> _streamKeepAliveLimit
  , ("commit_timeout", ) . encodeUtf8 . tshow . unCommitTimeout <$> _commitTimeout
  ]

newConfigFromEnv :: (MonadIO m, MonadThrow m, MonadMask b, MonadIO b) => m (Config b)
newConfigFromEnv = do
  request <- liftIO (lookupEnv "NAKADI_URL") >>= maybe (throwM NakadiUrlMissing) parseRequest
  pure $ newConfig httpBackendIO request

-- | Default retry policy.
defaultRetryPolicy :: MonadIO m => RetryPolicyM m
defaultRetryPolicy = fullJitterBackoff 2 <> limitRetries 5

-- | Default commit strategy.
defaultCommitStrategy :: CommitStrategy
defaultCommitStrategy = CommitSync

-- | Default worker configuration. This specifies single-threaded consumption of subscriptions.
defaultWorkerConfig :: WorkerConfig
defaultWorkerConfig = WorkerConfig { _nThreads = 1 }

-- | Producs a new configuration, with mandatory HTTP manager, default
-- consumption parameters and HTTP request template.
newConfig
  :: Monad b
  => HttpBackend b
  -> Request           -- ^ Request Template
  -> Config b          -- ^ Resulting Configuration
newConfig httpBackend request = Config { _manager                        = Nothing
                                       , _requestTemplate                = request
                                       , _requestModifier                = pure
                                       , _deserializationFailureCallback = Nothing
                                       , _streamConnectCallback          = Nothing
                                       , _logFunc                        = Nothing
                                       , _retryPolicy                    = defaultRetryPolicy
                                       , _http                           = httpBackend
                                       , _httpErrorCallback              = Nothing
                                       , _flowId                         = Nothing
                                       , _commitStrategy                 = defaultCommitStrategy
                                       , _commitTimeout                  = Nothing
                                       , _subscriptionStats              = Nothing
                                       , _maxUncommittedEvents           = Nothing
                                       , _batchLimit                     = Nothing
                                       , _streamLimit                    = Nothing
                                       , _batchFlushTimeout              = Nothing
                                       , _streamTimeout                  = Nothing
                                       , _streamKeepAliveLimit           = Nothing
                                       , _worker                         = defaultWorkerConfig
                                       }
