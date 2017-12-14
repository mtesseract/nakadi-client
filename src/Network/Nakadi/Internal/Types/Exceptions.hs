{-|
Module      : Network.Nakadi.Internal.Types.Exceptions
Description : Nakadi Client Exceptions (Internal)
Copyright   : (c) Moritz Schulte 2017
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX

Nakadi client exceptions.
-}

module Network.Nakadi.Internal.Types.Exceptions where

import           Network.Nakadi.Internal.Prelude

import qualified Data.ByteString.Lazy            as ByteString.Lazy
import           Network.Nakadi.Types.Problem
import           Network.Nakadi.Types.Service

data NakadiException = BatchPartiallySubmitted [BatchItemResponse]
                     | BatchNotSubmitted [BatchItemResponse]
                     | BatchValidationFailure [BatchItemResponse]
                     | ClientNotAuthenticated Problem
                     | AccessForbidden Problem
                     | UnprocessableEntity Problem
                     | Conflict Problem
                     | DeserializationFailure ByteString.Lazy.ByteString
                     | UnexpectedResponse (Response ())
                     | NotFound Problem
                     | TooManyRequests Problem
                     | BadRequest Problem
                     | SubscriptionNotFound Problem
                     | CursorAlreadyCommitted CursorCommitResults
                     | CursorResetInProgress Problem
                     | EventTypeNotFound Problem
                     | SubscriptionExistsAlready Subscription
                     | RequestModificationException SomeException
                     | CursorDistanceNoResult
                     deriving (Show, Typeable)

instance Exception NakadiException
