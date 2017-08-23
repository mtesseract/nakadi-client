{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}

module Nakadi.Internal.Config where

import           Nakadi.Internal.Prelude
import           Nakadi.Types


buildConsumeQueryParameters :: ConsumeParameters -> [(ByteString, ByteString)]
buildConsumeQueryParameters ConsumeParameters { .. } = catMaybes
  [ ("batch_limit",) . encodeUtf8 . tshow <$> _batchLimit
  , ("stream_limit",) . encodeUtf8 . tshow <$> _streamLimit
  , ("batch_flush_timeout",) . encodeUtf8 . tshow <$> _batchFlushTimeout
  , ("stream_timeout",) . encodeUtf8 . tshow <$> _streamTimeout
  , ("stream_keep_alive_limit",) . encodeUtf8 . tshow <$> _streamKeepAliveLimit ]

buildSubscriptionConsumeQueryParameters :: ConsumeParameters -> [(ByteString, ByteString)]
buildSubscriptionConsumeQueryParameters params@ConsumeParameters { .. } =
  buildConsumeQueryParameters params
  ++ catMaybes [("max_uncommitted_events",) . encodeUtf8 . tshow <$> _maxUncommittedEvents]
