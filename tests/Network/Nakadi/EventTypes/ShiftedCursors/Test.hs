{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}

module Network.Nakadi.EventTypes.ShiftedCursors.Test where

import           ClassyPrelude

import           Data.UUID                   ()
import           Network.Nakadi
import           Network.Nakadi.Tests.Common
import           System.Random
import           Test.Tasty
import           Test.Tasty.HUnit

testEventTypesShiftedCursors :: Config App -> TestTree
testEventTypesShiftedCursors conf = testGroup "ShiftedCursors"
  [ testCase "ShiftedCursorsZero" (testShiftedCursorsZero conf)
  , testCase "ShiftedCursorsN" (testShiftedCursorsN conf 10)
  ]

testShiftedCursorsZero :: Config App -> Assertion
testShiftedCursorsZero conf = runApp . runNakadiT conf $ do
  recreateEvent myEventTypeName myEventType
  partitions <- eventTypePartitions myEventTypeName
  let cursors = map extractCursor partitions
  cursors' <- cursorsShift myEventTypeName cursors 0
  liftIO $ cursors @=? cursors'

testShiftedCursorsN :: Config App -> Int64 -> Assertion
testShiftedCursorsN conf n = runApp . runNakadiT conf $ do
  now <- liftIO getCurrentTime
  eid <- EventId <$> liftIO randomIO
  recreateEvent myEventTypeName myEventType
  partitions <- eventTypePartitions myEventTypeName
  let cursors = map extractCursor partitions
  liftIO $ length cursors > 0 @=? True
  forM_ [1..n] $ \_ -> do
    eventsPublish myEventTypeName [myDataChangeEvent eid now]
  cursors' <- cursorsShift myEventTypeName cursors n
  liftIO $ length cursors' @=? length cursors
  forM_ (zip cursors cursors') $ \(c, c') -> do
    distance <- cursorDistance myEventTypeName c c'
    liftIO $ distance @=? n
