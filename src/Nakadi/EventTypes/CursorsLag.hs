{-# LANGUAGE TupleSections #-}

module Nakadi.EventTypes.CursorsLag
  ( eventTypeCursorsLag'
  , eventTypeCursorsLag
  ) where

import           Nakadi.Internal.Prelude

import           Control.Arrow
import           Control.Lens
import qualified Data.Map.Strict         as Map
import           Nakadi.Internal.Http
import qualified Nakadi.Internal.Lenses  as L

path :: EventTypeName -> ByteString
path eventTypeName = "/event-types/" <> encodeUtf8 (unEventTypeName eventTypeName) <> "/cursors-lag"

-- | POST to /event-types/NAME/cursors-lag.
eventTypeCursorsLag' :: MonadNakadi m
                     => Config -> EventTypeName -> [Cursor] -> m [Partition]
eventTypeCursorsLag' config eventTypeName cursors =
  httpJsonBody config ok200 []
  (setRequestMethod "POST" . setRequestPath (path eventTypeName) . setRequestBodyJSON cursors)

eventTypeCursorsLag :: MonadNakadi m
                    => Config
                    -> EventTypeName
                    -> Map PartitionName CursorOffset
                    -> m (Map PartitionName Int64)
eventTypeCursorsLag config eventTypeName cursorsMap = do
  partitionStats <- eventTypeCursorsLag' config eventTypeName cursors
  return $ partitionStats & map ((view L.partition &&& view L.unconsumedEvents) >>> sequenceSnd)
                          & catMaybes
                          & Map.fromList
  where cursors = map (uncurry Cursor) (Map.toList cursorsMap)

sequenceSnd :: Functor f => (a, f b) -> f (a, b)
sequenceSnd (a, fb) = (a,) <$> fb
