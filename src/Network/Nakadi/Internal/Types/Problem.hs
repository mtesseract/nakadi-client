-- | Implementation of the error object described in RFC7807.

{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module Network.Nakadi.Internal.Types.Problem where

import           Data.Aeson
import           Data.HashMap.Lazy (HashMap)
import           Data.Int
import           Data.Text         (Text)
import           Prelude

import qualified Data.HashMap.Lazy as HashMap
import           Data.Maybe
import           GHC.Generics

data Problem = Problem
  { problemType     :: Text
  , problemTitle    :: Text
  , problemStatus   :: Int32
  , problemDetail   :: Maybe Text
  , problemInstance :: Maybe Text
  , problemCustom   :: Maybe (HashMap Text Value)
  } deriving (Show, Eq, Generic)

instance ToJSON Problem where
  toJSON Problem { .. } =
    let hm = HashMap.fromList ([ ("type",   String problemType)
                               , ("title",  String problemTitle)
                               , ("status", Number (fromIntegral problemStatus)) ]
                               ++ catMaybes [ ("detail",)   . String <$> problemDetail
                                            , ("instance",) . String <$> problemInstance ])
    in Object (maybe hm (HashMap.union hm) problemCustom)

instance FromJSON Problem where
  parseJSON = withObject "Problem" $ \obj -> do
    let custom = HashMap.filterWithKey (\ k _ -> k `notElem` ["type", "title", "status"]) obj
    type'     <- obj .:  "type"
    title     <- obj .:  "title"
    status    <- obj .:  "status"
    detail    <- obj .:? "detail"
    instance' <- obj .:? "instance"
    pure Problem { problemType     = type'
                 , problemTitle    = title
                 , problemStatus   = status
                 , problemDetail   = detail
                 , problemInstance = instance'
                 , problemCustom   = if HashMap.null custom then Nothing else Just custom
                 }
