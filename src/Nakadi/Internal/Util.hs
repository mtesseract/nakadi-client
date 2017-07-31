{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Nakadi.Internal.Util
  ( conduitDrainToText
  , conduitDrainToLazyByteString
  , decodeThrow
  ) where

import           Nakadi.Internal.Prelude

import           Control.Lens
import           Data.Aeson
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy     as ByteString.Lazy
import           Data.Conduit
import           Data.Conduit.Combinators hiding (decodeUtf8)
import           Data.Text                (Text)
import           Data.Text.Strict.Lens

import           Nakadi.Types

conduitDrainToText :: (Monad m) => ConduitM () ByteString m () -> m Text
conduitDrainToText conduit = do
  b <- toLazyByteString <$> (conduit $$ sinkBuilder)
  return $ b^.strict.utf8

conduitDrainToLazyByteString :: (Monad m) => ConduitM () ByteString m () -> m ByteString.Lazy.ByteString
conduitDrainToLazyByteString conduit =
  toLazyByteString <$> (conduit $$ sinkBuilder)

decodeThrow :: (FromJSON a, MonadThrow m) => ByteString.Lazy.ByteString -> m a
decodeThrow s = case decode s of
  Just a  -> return a
  Nothing -> throwIO (DeserializationFailure s)
