{-|
Module      : Network.Nakadi.Internal.Json
Description : Nakadi Client Json (Internal)
Copyright   : (c) Moritz Schulte 2017
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX

Internal module containing JSON specific code.
-}

module Network.Nakadi.Internal.Json
  ( nakadiJsonOptions
  ) where

import           Prelude

import           Data.Aeson.Casing
import           Data.Aeson.Types
import           Data.Map          (Map)
import qualified Data.Map          as Map
import           Data.Maybe

nakadiJsonOptions :: Options
nakadiJsonOptions =
  let opts = aesonDrop 1 (fieldRenamer . snakeCase)
  in opts { omitNothingFields = True }

fieldRenameTable :: Map String String
fieldRenameTable = Map.fromList [ ("payload", "data") ]

fieldRenamer :: String -> String
fieldRenamer s = fromMaybe s $ Map.lookup s fieldRenameTable
