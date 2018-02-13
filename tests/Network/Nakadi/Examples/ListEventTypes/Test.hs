{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Network.Nakadi.Examples.ListEventTypes.Test
  ( testListEventTypes
  ) where

import           ClassyPrelude
import           Control.Lens
import           Control.Monad.Logger
import qualified Network.Nakadi        as Nakadi
import qualified Network.Nakadi.Lenses as L

testListEventTypes :: Nakadi.ConfigIO -> Bool -> IO ()
testListEventTypes config withLogging =
  runStdoutLoggingT . filterLogger (\ _ _ -> withLogging) $ dumpEventTypes config

dumpEventTypes :: Nakadi.ConfigIO -> LoggingT IO ()
dumpEventTypes config = Nakadi.runNakadiT config $ do
  eventTypes <- Nakadi.eventTypesList
  forM_ eventTypes $ \ eventType ->
    logInfoN $ tshow (eventType^.L.name)
