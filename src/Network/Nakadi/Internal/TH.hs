{-# LANGUAGE TemplateHaskell #-}

module Network.Nakadi.Internal.TH where

import           Control.Lens
import           Data.Char
import           Data.Maybe
import           Language.Haskell.TH
import           Prelude
-- import           Language.Haskell.TH.Lens
-- import           Network.Nakadi.Types
-- import Language.Haskell.TH.Syntax hiding (lift)
import           Data.List

makeNakadiLenses :: Name -> DecsQ
makeNakadiLenses = makeLensesWith nakadiLensRules

-- | A 'FieldNamer' for 'classUnderscoreNoPrefixFields'.
nakadiLensNamer :: FieldNamer
nakadiLensNamer _ _ field = maybeToList $ do
  fieldUnprefixed <- stripPrefix "_" (nameBase field)
  let className  = "HasNakadi" ++ overHead toUpper fieldUnprefixed
      methodName = fieldUnprefixed
  return (MethodName (mkName className) (mkName methodName))

nakadiLensRules :: LensRules
nakadiLensRules =
  defaultFieldRules & lensField .~ nakadiLensNamer

overHead :: (a -> a) -> [a] -> [a]
overHead _ []     = []
overHead f (x:xs) = f x : xs
