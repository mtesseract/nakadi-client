{-|
Module      : Network.Nakadi.Internal.Config
Description : Nakadi Client Configuration (Internal)
Copyright   : (c) Moritz Clasmeier 2017, 2018
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

buildConsumeQueryParameters :: Config m -> [(ByteString, ByteString)]
buildConsumeQueryParameters Config {..} = catMaybes
  [ ("batch_limit", ) . encodeUtf8 . tshow <$> _batchLimit
  , ("stream_limit", ) . encodeUtf8 . tshow <$> _streamLimit
  , ("batch_flush_timeout", ) . encodeUtf8 . tshow <$> _batchFlushTimeout
  , ("stream_timeout", ) . encodeUtf8 . tshow <$> _streamTimeout
  , ("max_uncommitted_events", ) . encodeUtf8 . tshow <$> _maxUncommittedEvents
  , ("stream_keep_alive_limit", ) . encodeUtf8 . tshow <$> _streamKeepAliveLimit
  ]
