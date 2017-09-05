module Network.Nakadi.Internal.Json (nakadiJsonOptions) where

import           Data.Aeson.Casing
import           Data.Aeson.Types
import           Data.Map          (Map)
import qualified Data.Map          as Map
import           Data.Maybe
import           Prelude

nakadiJsonOptions :: Options
nakadiJsonOptions =
  let opts = aesonDrop 1 (fieldRenamer . snakeCase)
  in opts { omitNothingFields = True }

fieldRenameTable :: Map String String
fieldRenameTable = Map.fromList [ ("payload", "data") ]

fieldRenamer :: String -> String
fieldRenamer s = fromMaybe s $ Map.lookup s fieldRenameTable
