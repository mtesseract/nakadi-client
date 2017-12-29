{-# LANGUAGE FlexibleContexts #-}

module Network.Nakadi.MonadicAPI.Test where

import           ClassyPrelude
import qualified Network.Nakadi   as Nakadi
import           Test.Tasty
import           Test.Tasty.HUnit
import Control.Monad.Trans.Resource

testMonadicAPI :: Nakadi.MonadNakadi b IO => Nakadi.Config' b -> TestTree
testMonadicAPI conf = testGroup "Monadic API"
  [ testCase "Simple Monadic API" (testSimpleMonadicAPI conf) ]

testSimpleMonadicAPI :: Nakadi.MonadNakadi b IO => Nakadi.Config' b -> Assertion
testSimpleMonadicAPI conf = Nakadi.runNakadiT conf $ do
  -- Tests Nakadi call within NakadiT monad transformer
  _events <- Nakadi.eventTypesListR
  -- But we can also call Nakadi from within monad transformers on top of NakadiT:
  flip runReaderT () $ do
    _events <- Nakadi.eventTypesListR
    pure ()
  -- Required for consuming via subscription API is the ability to
  -- call Nakadi from within ResourceT:
  runResourceT $ do
    _partitionStrategies <- Nakadi.registryPartitionStrategiesR
    pure ()
  -- We can retrieve the configuration using nakadiAsk:
  _conf <- Nakadi.nakadiAsk
  pure ()
