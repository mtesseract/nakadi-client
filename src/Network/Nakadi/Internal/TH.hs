 {-|
Module      : Network.Nakadi.Internal.TH
Description : Nakadi Client TemplateHaskell (Internal)
Copyright   : (c) Moritz Schulte 2017
License     : BSD2
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX

Internal TemplateHaskell specific code.
-}

{-# LANGUAGE TemplateHaskell #-}

module Network.Nakadi.Internal.TH where

import           Control.Lens
import           Data.Char
import           Data.List
import           Data.Maybe
import           Language.Haskell.TH
import           Prelude

-- | Create lenses for nakadi-client via Template Haskell.
makeNakadiLenses :: Name -> DecsQ
makeNakadiLenses = makeLensesWith nakadiLensRules

-- | A 'FieldNamer' for 'classUnderscoreNoPrefixFields'.
nakadiLensNamer :: FieldNamer
nakadiLensNamer _ _ field = maybeToList $ do
  fieldUnprefixed <- stripPrefix "_" (nameBase field)
  let className  = "HasNakadi" ++ overHead toUpper fieldUnprefixed
      methodName = fieldUnprefixed
  return (MethodName (mkName className) (mkName methodName))

-- | Rules for creating nakadi-client lenses.
nakadiLensRules :: LensRules
nakadiLensRules =
  defaultFieldRules & lensField .~ nakadiLensNamer

-- | Convenience function, copied from the lens package.
--
-- License: BSD2
-- Copyright (C) 2012-2016 Edward A. Kmett
overHead :: (a -> a) -> [a] -> [a]
overHead _ []     = []
overHead f (x:xs) = f x : xs
