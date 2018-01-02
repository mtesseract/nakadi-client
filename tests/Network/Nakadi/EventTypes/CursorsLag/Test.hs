{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}

module Network.Nakadi.EventTypes.CursorsLag.Test where

import           ClassyPrelude

import qualified Data.Map                    as Map
import           Network.Nakadi
import           Network.Nakadi.Tests.Common
import           Test.Tasty
import           Test.Tasty.HUnit

testEventTypesCursorsLag :: Config' App -> TestTree
testEventTypesCursorsLag conf = testGroup "CursorsLag"
  [ testCase "CursorsLagZero" (testCursorsLagZero conf)
  , testCase "CursorsLag10" (testCursorsLagN conf 10)
  ]

testCursorsLagZero :: Config' App -> Assertion
testCursorsLagZero conf = runApp . runNakadiT conf $ do
  partitions <- eventTypePartitionsR myEventTypeName
  let cursorsMap = Map.fromList $
        map (\Partition { .. } -> (_partition, _newestAvailableOffset)) partitions
  lagMap <- cursorsLagR myEventTypeName cursorsMap
  recreateEvent myEventTypeName myEventType
  liftIO $ do
    Map.size cursorsMap @=? Map.size lagMap
    forM_ (Map.toList lagMap) $ \(_, lag) ->
      liftIO $ lag @=? 0

testCursorsLagN :: Config' App -> Int64 -> Assertion
testCursorsLagN conf n = runApp . runNakadiT conf $ do
  now <- liftIO getCurrentTime
  eid <- EventId <$> liftIO genRandomUUID
  recreateEvent myEventTypeName myEventType
  partitions <- eventTypePartitionsR myEventTypeName
  let cursorsMap = Map.fromList $
        map (\Partition { .. } -> (_partition, _newestAvailableOffset)) partitions
  forM_ [1..n] $ \_ ->
    eventPublishR myEventTypeName Nothing [myDataChangeEvent eid now]
  lagMap <- cursorsLagR myEventTypeName cursorsMap
  liftIO $ Map.size cursorsMap @=? Map.size lagMap
  let lag = sum $ map snd (Map.toList lagMap)
  liftIO $ n @=? lag
