{-# LANGUAGE FlexibleContexts #-}

module Network.Nakadi.MonadicAPI.Test where

import           ClassyPrelude
import           Control.Monad.Trans.Resource
import           Control.Retry
import           Network.HTTP.Client
import qualified Network.Nakadi               as Nakadi
import           Network.Nakadi.Tests.Common
import           Test.Tasty
import           Test.Tasty.HUnit

httpErrorCallback :: Request -> HttpException -> RetryStatus -> Bool -> App ()
httpErrorCallback _req exn _retryStatus _isFinalFailure =
  putStrLn $ "nakadi-client triggered exception: " <> tshow exn

testMonadicAPI :: Nakadi.Config' App -> TestTree
testMonadicAPI conf = testGroup "Monadic API"
  [ testCase "Simple Monadic API" (testSimpleMonadicAPI conf') ]

  where conf' = Nakadi.setHttpErrorCallback httpErrorCallback conf

testSimpleMonadicAPI :: Nakadi.Config' App -> Assertion
testSimpleMonadicAPI conf = runApp . Nakadi.runNakadiT conf $ do
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
