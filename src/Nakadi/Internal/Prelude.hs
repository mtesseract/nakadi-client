-- | Custom Prelude on top of Prelude

module Nakadi.Internal.Prelude
  ( module Prelude
  , module Data.Int
  , module Data.Monoid
  , module Data.Maybe
  , module Control.Monad
  , module Control.Exception.Safe
  , ByteString
  , Text
  , Map
  , tshow
  , encodeUtf8
  , decodeUtf8
  , identity
  , MonadIO
  , Request
  , Response
  ) where

import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.ByteString
import           Data.Int
import           Data.Map               (Map)
import           Data.Maybe
import           Data.Monoid
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Data.Text.Encoding
import           Network.HTTP.Client    (Request, Response)
import           Prelude                hiding (id)

tshow :: Show a => a -> Text
tshow = Text.pack . show

identity :: a -> a
identity x = x
