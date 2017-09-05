{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Network.Nakadi.Subscriptions.Cursors
  ( subscriptionCursorCommit'
  , subscriptionCommit
  , subscriptionCommitOne
  , subscriptionCursors
  , subscriptionCursorsReset
  ) where

import           Network.Nakadi.Internal.Prelude

import           Data.Aeson

import qualified Control.Exception.Safe              as Safe
import           Control.Lens
import           Control.Monad.Reader
import qualified Data.HashMap.Lazy                   as HashMap
import           Network.Nakadi.Internal.Conversions
import           Network.Nakadi.Internal.Http
import           Network.Nakadi.Internal.Lenses      (HasSubscriptionCursor)
import qualified Network.Nakadi.Internal.Lenses      as L
import           Network.Nakadi.Internal.Types

path :: SubscriptionId -> ByteString
path subscriptionId = "/subscriptions/" <> subscriptionIdToByteString subscriptionId <> "/cursors"

-- | POST to /subscriptions/ID/cursors.
subscriptionCursorCommit' :: MonadNakadi m
                          => Config
                          -> SubscriptionId
                          -> StreamId
                          -> SubscriptionCursorCommit
                          -> m ()
subscriptionCursorCommit' config subscriptionId streamId cursors =
  httpJsonNoBody config status204 [(ok200, errorCursorAlreadyCommitted)]
  (setRequestMethod "POST"
   . addRequestHeader "X-Nakadi-StreamId" (encodeUtf8 (unStreamId streamId))
   . setRequestBodyJSON cursors
   . setRequestPath (path subscriptionId))

subscriptionCommit :: (MonadNakadi m, MonadCatch m, HasSubscriptionCursor a)
                   => [a]
                   -> ReaderT SubscriptionEventStreamContext m ()
subscriptionCommit as = do
  SubscriptionEventStreamContext { .. } <- ask
  Safe.catchJust
    exceptionPredicate
    (subscriptionCursorCommit' _config _subscriptionId _streamId cursorsCommit)
    (const (return ()))

  where exceptionPredicate = \case
          CursorAlreadyCommitted _ -> Just ()
          _ -> Nothing

        cursors = map (^. L.subscriptionCursor) as
        cursorsCommit = SubscriptionCursorCommit cursors

subscriptionCommitOne :: (MonadNakadi m, MonadCatch m, HasSubscriptionCursor a)
                      => a
                      -> ReaderT SubscriptionEventStreamContext m ()
subscriptionCommitOne a = subscriptionCommit [a]

subscriptionCursors :: MonadNakadi m
                    => Config
                    -> SubscriptionId
                    -> m [SubscriptionCursor]
subscriptionCursors config subscriptionId =
  httpJsonBody config ok200 []
  (setRequestMethod "GET" . setRequestPath (path subscriptionId))

subscriptionCursorsReset :: MonadNakadi m
                         => Config
                         -> SubscriptionId
                         -> [SubscriptionCursorWithoutToken]
                         -> m ()
subscriptionCursorsReset config subscriptionId cursors =
  httpJsonNoBody config status204 [ (status404, errorSubscriptionNotFound)
                                 , (status409, errorCursorResetInProgress) ]
  (setRequestMethod "PATCH"
   . setRequestPath (path subscriptionId)
   . setRequestBodyJSON (Object (HashMap.fromList [("items", toJSON cursors)])))
