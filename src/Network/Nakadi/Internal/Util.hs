module Network.Nakadi.Internal.Util
  ( conduitDrainToLazyByteString
  , decodeThrow
  ) where

import           Network.Nakadi.Internal.Prelude

import           Data.Aeson
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy            as ByteString.Lazy
import           Data.Conduit
import           Data.Conduit.Combinators        hiding (decodeUtf8)

import           Network.Nakadi.Types

conduitDrainToLazyByteString :: Monad m
                             => ConduitM () ByteString m ()
                             -> m ByteString.Lazy.ByteString
conduitDrainToLazyByteString conduit =
  toLazyByteString <$> (conduit $$ sinkBuilder)

decodeThrow :: (FromJSON a, MonadThrow m) => ByteString.Lazy.ByteString -> m a
decodeThrow s = case decode s of
  Just a  -> return a
  Nothing -> throwIO (DeserializationFailure s)
