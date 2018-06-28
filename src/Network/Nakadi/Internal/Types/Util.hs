{-|
Module      : Network.Nakadi.Internal.Types.Util
Description : Nakadi Client Utilities for Types (Internal)
Copyright   : (c) Moritz Clasmeier 2017, 2018
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX

Internal utility functions used for type definitions.
-}

module Network.Nakadi.Internal.Types.Util where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text                      ( Text )
import           Data.Maybe
import           Data.Scientific                ( toBoundedInteger )
import qualified Data.Text                     as Text
import           Data.UUID
import           Prelude

-- | Construct a field renamer function from a field renamer map.
makeFieldRenamer :: [(String, String)] -> String -> String
makeFieldRenamer fieldMap field = fromMaybe field (lookup field fieldMap)

parseUUID :: String -> (UUID -> a) -> Value -> Parser a
parseUUID label constructor val@(String valS) =
  case Data.UUID.fromString (Text.unpack valS) of
    Just uuid -> return (constructor uuid)
    Nothing   -> typeMismatch label val
parseUUID label _ val = typeMismatch label val

parseInteger
  :: (Integral i, Bounded i) => String -> (i -> a) -> Value -> Parser a
parseInteger label constructor val@(Number n) = case toBoundedInteger n of
  Just i  -> return $ constructor i
  Nothing -> typeMismatch label val
parseInteger label _ val = typeMismatch label val

parseString :: String -> (Text -> a) -> Value -> Parser a
parseString _label ctor (String s) = pure (ctor s)
parseString label  _    val        = typeMismatch label val
