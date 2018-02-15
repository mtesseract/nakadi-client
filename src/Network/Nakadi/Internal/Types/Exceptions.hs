{-|
Module      : Network.Nakadi.Internal.Types.Exceptions
Description : Nakadi Client Exceptions (Internal)
Copyright   : (c) Moritz Clasmeier 2017, 2018
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX

Nakadi client exceptions.
-}

{-# LANGUAGE StrictData #-}

module Network.Nakadi.Internal.Types.Exceptions where

import           Network.Nakadi.Internal.Prelude

import           Network.Nakadi.Types.Problem
import           Network.Nakadi.Types.Service

data NakadiException = BatchPartiallySubmitted [BatchItemResponse]
                     | BatchNotSubmitted [BatchItemResponse]
                     | BatchValidationFailure [BatchItemResponse]
                     | ClientNotAuthenticated Problem
                     | AccessForbidden Problem
                     | UnprocessableEntity Problem
                     | Conflict Problem
                     | DeserializationFailure ByteString Text
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
                     | StreamIdMissing
                     deriving (Show, Typeable)

instance Exception NakadiException
