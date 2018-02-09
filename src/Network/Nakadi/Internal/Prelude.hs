{-|
Module      : Network.Nakadi.Prelude
Description : Nakadi Client Prelude (Internal)
Copyright   : (c) Moritz Schulte 2017
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX

Internal custom Prelude.
-}

module Network.Nakadi.Internal.Prelude
  ( module Prelude
  , module Data.Int
  , module Data.Monoid
  , module Data.Maybe
  , module Control.Monad
  , module Control.Exception.Safe
  , module Control.Monad.Reader.Class
  , ByteString
  , Text
  , Map
  , tshow
  , encodeUtf8
  , decodeUtf8
  , identity
  , undefined
  , error
  , MonadIO
  , liftIO
  , Request
  , Response
  ) where

import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader.Class
import           Data.ByteString
import           Data.Int
import           Data.Map                   (Map)
import           Data.Maybe
import           Data.Monoid
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Data.Text.Encoding
import           Network.HTTP.Client        (Request, Response)
import           Prelude                    hiding (error, id, undefined)
import qualified Prelude

tshow :: Show a => a -> Text
tshow = Text.pack . show

identity :: a -> a
identity x = x

{-# WARNING undefined "usage of 'undefined' forbidden" #-}
undefined :: a
undefined = Prelude.undefined

{-# WARNING error "usage of 'error' forbidden" #-}
error :: String -> a
error = Prelude.error
