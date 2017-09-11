-- | HTTP Client Convenience Functions.

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Network.Nakadi.Internal.Http
  ( module Network.HTTP.Simple
  , module Network.HTTP.Types.Status
  , module Network.Nakadi.Internal.Types
  , httpJsonBody
  , httpJsonNoBody
  , httpJsonBodyStream
  , errorClientNotAuthenticated
  , errorUnprocessableEntity
  , errorAccessForbidden
  , errorTooManyRequests
  , errorBadRequest
  , errorSubscriptionNotFound
  , errorCursorAlreadyCommitted
  , errorCursorResetInProgress
  , errorEventTypeNotFound
  , errorSubscriptionExistsAlready
  , setRequestQueryParameters
  ) where

import           Network.Nakadi.Internal.Prelude

import           Conduit
import           Control.Arrow
import           Control.Lens
import           Control.Monad                   (void)
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Aeson
import qualified Data.ByteString.Lazy            as ByteString.Lazy
import qualified Data.Conduit.Binary             as Conduit
import qualified Data.Text                       as Text
import           Network.HTTP.Client             (responseClose, responseOpen)
import           Network.HTTP.Client.Conduit     (bodyReaderSource)
import           Network.HTTP.Simple
import           Network.HTTP.Types
import           Network.HTTP.Types.Status
import qualified Network.Nakadi.Internal.Lenses  as L
import           Network.Nakadi.Internal.Types
import           Network.Nakadi.Internal.Util

conduitDecode :: (MonadIO m, FromJSON a) => Config -> ConduitM ByteString a m ()
conduitDecode Config { .. } = awaitForever $ \a ->
  case decodeStrict a of
    Just v  -> yield v
    Nothing -> liftIO $ callback a

    where callback = fromMaybe (const (return ())) _deserializationFailureCallback

httpBuildRequest :: (MonadIO m, MonadThrow m, MonadCatch m) => Config -> (Request -> Request) -> m Request
httpBuildRequest Config { .. } requestDef = do
  let manager = _manager
      request = requestDef _requestTemplate & setRequestManager manager
  tryAny (liftIO (_requestModifier request)) >>= \case
    Right modifiedRequest -> return modifiedRequest
    Left  exn             -> throwIO $ RequestModificationException exn

httpExecRequest :: (MonadIO m, MonadThrow m, MonadCatch m)
                => Config
                -> (Request -> Request)
                -> m (Response ByteString.Lazy.ByteString)
httpExecRequest config requestDef = httpBuildRequest config requestDef >>= (liftIO . httpLbs)

httpExecRequestWithStatus :: (MonadIO m, MonadThrow m, MonadCatch m)
                          => Config
                          -> (Request -> Request)
                          -> m (Response ByteString.Lazy.ByteString, Status)
httpExecRequestWithStatus config requestDef =
  (identity &&& getResponseStatus) <$> httpExecRequest config requestDef

httpJsonBody :: (MonadThrow m, MonadCatch m, MonadIO m, FromJSON a)
             => Config
             -> Status
             -> [(Status, ByteString.Lazy.ByteString -> m LibException)]
             -> (Request -> Request)
             -> m a
httpJsonBody config successStatus exceptionMap requestDef = do
  (response, status) <- httpExecRequestWithStatus config requestDef
  if status == successStatus
    then decodeThrow (getResponseBody response)
    else case lookup status exceptionMap' of
           Just mkExn -> mkExn (getResponseBody response) >>= throwIO
           Nothing    -> throwIO (UnexpectedResponse (void response))

  where exceptionMap' = exceptionMap ++ defaultExceptionMap

httpJsonNoBody :: (MonadThrow m, MonadCatch m, MonadIO m)
             => Config
             -> Status
             -> [(Status, ByteString.Lazy.ByteString -> m LibException)]
             -> (Request -> Request)
             -> m ()
httpJsonNoBody config successStatus exceptionMap requestDef = do
  (response, status) <- httpExecRequestWithStatus config requestDef
  unless (status == successStatus) $
    case lookup status exceptionMap' of
      Just mkExn -> mkExn (getResponseBody response) >>= throwIO
      Nothing    -> throwIO (UnexpectedResponse (void response))

  where exceptionMap' = exceptionMap ++ defaultExceptionMap

httpJsonBodyStream :: (MonadMask m, MonadIO m, FromJSON a, MonadBaseControl IO m, MonadResource m)
                   => Config
                   -> Status
                   -> (Response () -> Either Text b)
                   -> [(Status, ByteString.Lazy.ByteString -> m LibException)]
                   -> (Request -> Request)
                   -> m (b, ConduitM () a (ReaderT r m) ())
httpJsonBodyStream config successStatus f exceptionMap requestDef = do
  let manager        = config^.L.manager
      request        = requestDef (config^.L.requestTemplate)
                       & setRequestManager manager
  (_, response) <- allocate (responseOpen request manager) responseClose
  let response_  = void response
      bodySource = bodyReaderSource (getResponseBody response)
      status     = getResponseStatus response
  if status == successStatus
    then do connectCallback (config^.L.logFunc) response_
            let conduit = bodySource
                          .| Conduit.lines
                          -- .| Conduit.iterM (liftIO . Text.putStrLn . Text.decodeUtf8)
                          .| conduitDecode config
            case f response_ of
              Left errMsg -> throwString (Text.unpack errMsg)
              Right b     -> return (b, readerC (const conduit))
    else case lookup status exceptionMap' of
           Just mkExn -> conduitDrainToLazyByteString bodySource >>= mkExn >>= throwIO
           Nothing    -> throwIO (UnexpectedResponse response_)

  where exceptionMap' = exceptionMap ++ defaultExceptionMap

        connectCallback = case config^.L.streamConnectCallback of
          Just cb -> \logFunc response -> liftIO $ cb logFunc response
          Nothing -> \_ _ -> return ()

setRequestQueryParameters :: [(ByteString, ByteString)] -> Request -> Request
setRequestQueryParameters parameters = setRequestQueryString parameters'
  where parameters' = map (fmap Just) parameters

defaultExceptionMap :: MonadThrow m
                    => [(Status, ByteString.Lazy.ByteString -> m LibException)]
defaultExceptionMap =
  [ (status401, errorClientNotAuthenticated)
  , (status403, errorAccessForbidden)
  , (status400, errorBadRequest)
  , (status422, errorUnprocessableEntity)
  , (status409, errorConflict) ]

errorClientNotAuthenticated :: MonadThrow m => ByteString.Lazy.ByteString -> m LibException
errorClientNotAuthenticated s = ClientNotAuthenticated <$> decodeThrow s

errorConflict :: MonadThrow m => ByteString.Lazy.ByteString -> m LibException
errorConflict s = Conflict <$> decodeThrow s

errorUnprocessableEntity :: MonadThrow m => ByteString.Lazy.ByteString -> m LibException
errorUnprocessableEntity s = UnprocessableEntity <$> decodeThrow s

errorAccessForbidden :: MonadThrow m => ByteString.Lazy.ByteString -> m LibException
errorAccessForbidden s = AccessForbidden <$> decodeThrow s

errorTooManyRequests :: MonadThrow m => ByteString.Lazy.ByteString -> m LibException
errorTooManyRequests s = TooManyRequests <$> decodeThrow s

errorBadRequest :: MonadThrow m => ByteString.Lazy.ByteString -> m LibException
errorBadRequest s = BadRequest <$> decodeThrow s

errorSubscriptionNotFound :: MonadThrow m => ByteString.Lazy.ByteString -> m LibException
errorSubscriptionNotFound s = SubscriptionNotFound <$> decodeThrow s

errorCursorAlreadyCommitted :: MonadThrow m => ByteString.Lazy.ByteString -> m LibException
errorCursorAlreadyCommitted s = CursorAlreadyCommitted <$> decodeThrow s

errorCursorResetInProgress :: MonadThrow m => ByteString.Lazy.ByteString -> m LibException
errorCursorResetInProgress s = CursorResetInProgress <$> decodeThrow s

errorEventTypeNotFound :: MonadThrow m => ByteString.Lazy.ByteString -> m LibException
errorEventTypeNotFound s = EventTypeNotFound <$> decodeThrow s

errorSubscriptionExistsAlready :: MonadThrow m => ByteString.Lazy.ByteString -> m LibException
errorSubscriptionExistsAlready s = SubscriptionExistsAlready <$> decodeThrow s
