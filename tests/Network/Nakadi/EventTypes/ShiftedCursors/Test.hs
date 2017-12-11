{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}

module Network.Nakadi.EventTypes.ShiftedCursors.Test where

import           ClassyPrelude

import           Network.Nakadi
import           Network.Nakadi.Tests.Common
import           Test.Tasty
import           Test.Tasty.HUnit

testEventTypesShiftedCursors :: Config -> TestTree
testEventTypesShiftedCursors conf = testGroup "ShiftedCursors"
  [ testCase "ShiftedCursorsZero" (testShiftedCursorsZero conf)
  , testCase "ShiftedCursorsN" (testShiftedCursorsN conf 10)
  ]

testShiftedCursorsZero :: Config -> Assertion
testShiftedCursorsZero conf = do
  recreateEvent conf myEventTypeName myEventType
  partitions <- eventTypePartitions conf myEventTypeName
  let cursors = map extractCursor partitions
  cursors' <- cursorsShift conf myEventTypeName cursors 0
  cursors @=? cursors'

testShiftedCursorsN :: Config -> Int64 -> Assertion
testShiftedCursorsN conf n = do
  now <- getCurrentTime
  eid <- tshow <$> genRandomUUID
  recreateEvent conf myEventTypeName myEventType
  partitions <- eventTypePartitions conf myEventTypeName
  let cursors = map extractCursor partitions
  length cursors > 0 @=? True
  forM_ [1..n] $ \_ -> do
    eventPublish conf myEventTypeName Nothing [myDataChangeEvent eid now]
  cursors' <- cursorsShift conf myEventTypeName cursors n
  length cursors' @=? length cursors
  forM_ (zip cursors cursors') $ \(c, c') -> do
    distance <- cursorDistance conf myEventTypeName c c'
    distance @=? n
