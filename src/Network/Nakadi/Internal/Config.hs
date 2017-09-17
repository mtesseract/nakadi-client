{-|
Module      : Network.Nakadi.Internal.Config
Description : Nakadi Client Configuration (Internal)
Copyright   : (c) Moritz Schulte 2017
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

import           Network.Nakadi.Internal.Prelude

import           Network.Nakadi.Internal.Types

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
