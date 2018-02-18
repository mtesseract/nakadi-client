{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Network.Nakadi.Examples.Echo.Echo (runEcho) where

import           ClassyPrelude
import           Conduit
import           Control.Concurrent.Async.Lifted (link, waitEither_)
import           Control.Lens
import           Data.Aeson
import           Data.Conduit.TQueue
import           Network.Nakadi
import qualified Network.Nakadi.Lenses           as L

-- Example program which consumes the events for the event type
-- "test-event" and republishes them unchanged under the event type
-- "test-event-copy".

runEcho :: EventTypeName -> EventTypeName -> NakadiT IO IO ()
runEcho eventNameInput eventNameOutput =
  runResourceT $ do
  channel :: TBQueue (Vector Value) <- atomically $ newTBQueue 1024
  consumer  <- async $ consumeEvents eventNameInput channel
  publisher <- async $ publishEvents eventNameOutput channel
  link consumer
  link publisher
  waitEither_ consumer publisher

  where consumeEvents eventName channel =
          eventsProcessConduit (Just consumeParameters) eventName Nothing $
          concatMapC (view L.events)
          .| mapC (fmap (toJSON :: DataChangeEvent Value -> Value))
          .| sinkTBQueue channel

        publishEvents eventName channel =
          sourceTBQueue channel
            .| mapC toList
            $$ mapM_C (eventsPublish eventName)

        consumeParameters = defaultConsumeParameters & L.batchFlushTimeout .~ Just 1
