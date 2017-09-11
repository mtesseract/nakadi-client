module Network.Nakadi.Internal.Conversions where

import           Network.Nakadi.Internal.Prelude

import           Control.Lens
import           Data.ByteString.Lens
import qualified Data.UUID                       as UUID
import qualified Network.Nakadi.Internal.Lenses  as L
import           Network.Nakadi.Types

subscriptionIdToByteString :: SubscriptionId -> ByteString
subscriptionIdToByteString = view (L.id.to UUID.toString.packedChars)
