-- |

module Nakadi.Internal.Conversions where

import           Nakadi.Internal.Prelude

import           Control.Lens
import           Data.ByteString.Lens
import qualified Data.UUID               as UUID
import qualified Nakadi.Internal.Lenses  as L
import           Nakadi.Types

subscriptionIdToByteString :: SubscriptionId -> ByteString
subscriptionIdToByteString = view (L.id.to UUID.toString.packedChars)
