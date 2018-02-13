{-|
Module      : Network.Nakadi.Internal.Http
Description : Nakadi Client HTTP (Internal)
Copyright   : (c) Moritz Schulte 2017, 2018
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX

Internal module containing HTTP client relevant code.
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Network.Nakadi.Internal.Http
  ( module Network.HTTP.Simple
  , module Network.HTTP.Types.Status
  , module Network.Nakadi.Internal.Types
  , httpJsonBody
  , httpJsonNoBody
  , httpJsonBodyStream
  , httpBuildRequest
  , conduitDecode
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
  , errorBatchPartiallySubmitted
  , errorBatchNotSubmitted
  , setRequestQueryParameters
  ) where

import           Network.Nakadi.Internal.Prelude

import           Conduit                         hiding (throwM)
import           Control.Arrow
import           Control.Lens
import           Control.Monad                   (void)
import           Data.Aeson
import qualified Data.ByteString.Lazy            as ByteString.Lazy
import qualified Data.ByteString.Lazy            as LB
import qualified Data.Text                       as Text
import           Network.HTTP.Client             (BodyReader,
                                                  HttpException (..),
                                                  HttpExceptionContent (..),
                                                  Manager, checkResponse,
                                                  responseBody, responseStatus)
import           Network.HTTP.Simple             hiding (Proxy)
import           Network.HTTP.Types
import           Network.HTTP.Types.Status
import qualified Network.Nakadi.Internal.Lenses  as L
import           Network.Nakadi.Internal.Types
import           Network.Nakadi.Internal.Util

conduitDecode
  :: (FromJSON a, MonadNakadi b m)
  => Config b -- ^ Configuration, containing the
              -- deserialization-failure-callback.
  -> ConduitM ByteString a m () -- ^ Conduit deserializing bytestrings
                                -- into custom values
conduitDecode Config {..} = awaitForever $ \a -> case eitherDecodeStrict a of
  Right v   -> yield v
  Left  err -> lift . nakadiLiftBase $ callback a (Text.pack err)
 where
  callback = fromMaybe dummyCallback _deserializationFailureCallback
  dummyCallback _ _ = return ()

-- | Throw 'HttpException' exception on server errors (5xx).
checkNakadiResponse :: Request -> Response BodyReader -> IO ()
checkNakadiResponse request response =
  when (statusCode (responseStatus response) `div` 100 == 5)
    $ throwIO
    $ HttpExceptionRequest request (StatusCodeException (void response) mempty)

httpBuildRequest
  :: MonadNakadi b m
  => (Request -> Request) -- ^ Pure request modifier
  -> m Request -- ^ Resulting request to execute
httpBuildRequest requestDef = do
  config <- nakadiAsk
  let request = requestDef (config ^. L.requestTemplate)
        & \req -> req { checkResponse = checkNakadiResponse }
  modifyRequest (config ^. L.requestModifier) request

-- | Modify the Request based on a user function in the configuration.
modifyRequest
  :: MonadNakadi b m => (Request -> b Request) -> Request -> m Request
modifyRequest rm request = tryAny (nakadiLiftBase (rm request)) >>= \case
  Right modifiedRequest -> return modifiedRequest
  Left  exn             -> throwIO $ RequestModificationException exn

-- | Executes an HTTP request using the provided configuration and a
-- pure request modifier.
httpExecRequest
  :: MonadNakadi b m
  => (Request -> Request)
  -> m (Response ByteString.Lazy.ByteString)
httpExecRequest requestDef = do
  config <- nakadiAsk
  req    <- httpBuildRequest requestDef
  nakadiLiftBase $ nakadiHttpLbs config req (config^.L.manager)

nakadiHttpLbs :: Config b
              -> Request
              -> Maybe Manager
              -> b (Response LB.ByteString)
nakadiHttpLbs config req maybeMngr =
  (config^.L.http.L.httpLbs) config req maybeMngr

-- | Executes an HTTP request using the provided configuration and a
-- pure request modifier. Returns the HTTP response and separately the
-- response status.
httpExecRequestWithStatus
  :: MonadNakadi b m
  => (Request -> Request) -- ^ Pure request modifier
  -> m (Response ByteString.Lazy.ByteString, Status)
httpExecRequestWithStatus requestDef =
  (identity &&& getResponseStatus) <$> httpExecRequest requestDef

httpJsonBody
  :: (MonadNakadi b m, FromJSON a)
  => Status
  -> [(Status, ByteString.Lazy.ByteString -> m NakadiException)]
  -> (Request -> Request)
  -> m a
httpJsonBody successStatus exceptionMap requestDef = do
  (response, status) <- httpExecRequestWithStatus requestDef
  if status == successStatus
    then decodeThrow (getResponseBody response)
    else case lookup status exceptionMap' of
      Just mkExn -> mkExn (getResponseBody response) >>= throwM
      Nothing    -> throwM (UnexpectedResponse (void response))
  where exceptionMap' = exceptionMap ++ defaultExceptionMap

httpJsonNoBody
  :: MonadNakadi b m
  => Status
  -> [(Status, ByteString.Lazy.ByteString -> m NakadiException)]
  -> (Request -> Request)
  -> m ()
httpJsonNoBody successStatus exceptionMap requestDef = do
  (response, status) <- httpExecRequestWithStatus requestDef
  unless (status == successStatus) $ case lookup status exceptionMap' of
    Just mkExn -> mkExn (getResponseBody response) >>= throwIO
    Nothing    -> throwIO (UnexpectedResponse (void response))
  where exceptionMap' = exceptionMap ++ defaultExceptionMap

nakadiHttpResponseOpen :: Config b
                       -> Request
                       -> Maybe Manager
                       -> b (Response (ConduitM () ByteString b ()))
nakadiHttpResponseOpen config req maybeMngr =
  (config^.L.http.L.httpResponseOpen) config req maybeMngr

nakadiHttpResponseClose :: Config b -> Response () -> b ()
nakadiHttpResponseClose config rsp =
  (config^.L.http.L.httpResponseClose) rsp

httpJsonBodyStream
  :: forall b m r
   . (MonadNakadi b m, MonadMask m)
  => Status
  -> [(Status, ByteString.Lazy.ByteString -> m NakadiException)]
  -> (Request -> Request)
  -> (Response (ConduitM () ByteString m ()) -> m r)
  -> m r
httpJsonBodyStream successStatus exceptionMap requestDef handler = do
  config  <- nakadiAsk
  request <- httpBuildRequest requestDef
  bracket (nakadiLiftBase $ nakadiHttpResponseOpen config request (config^.L.manager))
          (nakadiLiftBase . nakadiHttpResponseClose config . void)
   $ \response -> wrappedHandler config response

  where wrappedHandler :: Config b -> Response (ConduitM () ByteString b ()) -> m r
        wrappedHandler config response = do
          let response_      = void response
              status         = responseStatus response_
              bodySource     = responseBody responseLifted
              responseLifted = fmap (transPipe nakadiLiftBase) response
          if status == successStatus
            then do connectCallback config response_
                    handler responseLifted
            else case lookup status exceptionMap' of
                   Just mkExn ->
                     conduitDrainToLazyByteString bodySource >>= mkExn >>= throwM
                   Nothing -> throwM (UnexpectedResponse response_)

        exceptionMap' = exceptionMap ++ defaultExceptionMap

        -- connectCallback :: s -> t -> m ()
        connectCallback config response =
          nakadiLiftBase $ case config^.L.streamConnectCallback of
                         Just cb -> cb response
                         Nothing -> pure ()

setRequestQueryParameters :: [(ByteString, ByteString)] -> Request -> Request
setRequestQueryParameters parameters = setRequestQueryString parameters'
  where parameters' = map (fmap Just) parameters

defaultExceptionMap
  :: MonadThrow m => [(Status, ByteString.Lazy.ByteString -> m NakadiException)]
defaultExceptionMap =
  [ (status401, errorClientNotAuthenticated)
  , (status403, errorAccessForbidden)
  , (status400, errorBadRequest)
  , (status422, errorUnprocessableEntity)
  , (status409, errorConflict)
  ]

errorClientNotAuthenticated
  :: MonadThrow m => ByteString.Lazy.ByteString -> m NakadiException
errorClientNotAuthenticated s = ClientNotAuthenticated <$> decodeThrow s

errorConflict :: MonadThrow m => ByteString.Lazy.ByteString -> m NakadiException
errorConflict s = Conflict <$> decodeThrow s

errorUnprocessableEntity
  :: MonadThrow m => ByteString.Lazy.ByteString -> m NakadiException
errorUnprocessableEntity s = UnprocessableEntity <$> decodeThrow s

errorAccessForbidden
  :: MonadThrow m => ByteString.Lazy.ByteString -> m NakadiException
errorAccessForbidden s = AccessForbidden <$> decodeThrow s

errorTooManyRequests
  :: MonadThrow m => ByteString.Lazy.ByteString -> m NakadiException
errorTooManyRequests s = TooManyRequests <$> decodeThrow s

errorBadRequest
  :: MonadThrow m => ByteString.Lazy.ByteString -> m NakadiException
errorBadRequest s = BadRequest <$> decodeThrow s

errorSubscriptionNotFound
  :: MonadThrow m => ByteString.Lazy.ByteString -> m NakadiException
errorSubscriptionNotFound s = SubscriptionNotFound <$> decodeThrow s

errorCursorAlreadyCommitted
  :: MonadThrow m => ByteString.Lazy.ByteString -> m NakadiException
errorCursorAlreadyCommitted s = CursorAlreadyCommitted <$> decodeThrow s

errorCursorResetInProgress
  :: MonadThrow m => ByteString.Lazy.ByteString -> m NakadiException
errorCursorResetInProgress s = CursorResetInProgress <$> decodeThrow s

errorEventTypeNotFound
  :: MonadThrow m => ByteString.Lazy.ByteString -> m NakadiException
errorEventTypeNotFound s = EventTypeNotFound <$> decodeThrow s

errorSubscriptionExistsAlready
  :: MonadThrow m => ByteString.Lazy.ByteString -> m NakadiException
errorSubscriptionExistsAlready s = SubscriptionExistsAlready <$> decodeThrow s

errorBatchPartiallySubmitted
  :: MonadThrow m => ByteString.Lazy.ByteString -> m NakadiException
errorBatchPartiallySubmitted s = BatchPartiallySubmitted <$> decodeThrow s

errorBatchNotSubmitted
  :: MonadThrow m => ByteString.Lazy.ByteString -> m NakadiException
errorBatchNotSubmitted s = BatchNotSubmitted <$> decodeThrow s
