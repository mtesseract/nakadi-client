{-|
Module      : Network.Nakadi.Internal.Util
Description : Nakadi Client Utilities (Internal)
Copyright   : (c) Moritz Clasmeier 2017, 2018
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX

Internal utility functions.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}

module Network.Nakadi.Internal.Util
  ( conduitDrainToLazyByteString
  , decodeThrow
  , sequenceSnd
  , extractQueryParametersFromPath
  ) where

import           Network.Nakadi.Internal.Prelude

import           Data.Aeson
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy            as ByteString.Lazy
import           Data.Conduit
import           Data.Conduit.Combinators        hiding (decodeUtf8, map)
import qualified Data.Text                       as Text
import           Network.HTTP.Simple

import           Network.Nakadi.Internal.Types

conduitDrainToLazyByteString ::
  Monad b
  => ConduitM () ByteString b ()
  -> b ByteString.Lazy.ByteString
conduitDrainToLazyByteString conduit =
  toLazyByteString <$> (conduit $$ sinkBuilder)

decodeThrow
  :: (FromJSON a, MonadThrow m)
  => ByteString.Lazy.ByteString
  -> m a
decodeThrow s = case eitherDecode' s of
  Right a  -> pure a
  Left err -> throwM (DeserializationFailure (ByteString.Lazy.toStrict s) (Text.pack err))

sequenceSnd :: Functor f => (a, f b) -> f (a, b)
sequenceSnd (a, fb) = (a,) <$> fb

extractQueryParametersFromPath :: String -> Maybe [(ByteString, ByteString)]
extractQueryParametersFromPath path =
  case parseRequest ("http://localhost" <> path) of
    Just req -> Just . catMaybes . map sequenceSnd . getRequestQueryString $ req
    Nothing -> Nothing
