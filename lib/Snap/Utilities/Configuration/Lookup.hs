{-# LANGUAGE OverloadedStrings #-}

module Snap.Utilities.Configuration.Lookup (
	cfgLookup,
    cfgLookupWithDefault,
	stringValue,
	listValue
) where

import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT
import Data.List (groupBy, intercalate, find, sortBy)
import Data.HashMap.Strict (toList)
import Data.Text (Text, isPrefixOf, splitOn, pack, unpack)

import Snap.Utilities.Configuration.Types

------------------------------------------------------------------------------
-- | Look up a value.
cfgLookup :: Text -> (CT.Value -> a) -> [ConfigPair] -> Maybe a
cfgLookup key transformer = fmap (transformer . snd) . (found key)

-- | Look up a value and fall back to a default.
cfgLookupWithDefault :: Text -> a -> (CT.Value -> a) -> [ConfigPair] -> a
cfgLookupWithDefault key def transformer = maybe def (transformer . snd) . (found key)

-- | Internal: find a ConfigPair matching the key.
found :: Text -> [ConfigPair] -> Maybe ConfigPair
found key = find (\(k, _) -> k == key)

-- | Show a Configurator value as a String.
stringValue :: CT.Value -> String
stringValue (CT.String x) = unpack x
stringValue (CT.List x) = intercalate " " $ map show x
stringValue x = show x

-- | Show a Configurator value as a list.
listValue :: CT.Value -> [String]
listValue (CT.List x)   = map stringValue x
listValue (CT.String x) = map unpack . splitOn "," $ x
listValue x = listValue . CT.String . pack . show $ x
