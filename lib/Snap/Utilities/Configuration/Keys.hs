{-# LANGUAGE OverloadedStrings #-}

module Snap.Utilities.Configuration.Keys (
	keyPre,
	rebaseKey,
	groupName
) where

import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT
import Data.List (groupBy, intercalate, find, sortBy)
import Data.HashMap.Strict (toList)
import Data.Text (Text, isPrefixOf, splitOn, pack, unpack)

import Snap.Utilities.Configuration.Types

------------------------------------------------------------------------------
-- | Get the prefix for a AuthDomain key.
keyPre :: Text -> [Text]
keyPre k = take l $ splitK k
  where
  	l = length (splitK k) - 1

-- | Reduce an AuthDomain key down to its last element.
rebaseKey :: ConfigPair -> ConfigPair
rebaseKey (k, v) = (head . drop l . splitK $ k, v)
  where
  	l = length (splitK k) - 1

-- | Get a name for a group of items.
-- Has to drop 1 because the first item is always the type of group.
groupName :: [ConfigPair] -> String
groupName = unpack . head . drop 1 . keyPre . fst . head

-- | Splits a Configurator key up by dot delimiter.
splitK :: Text -> [Text]
splitK = splitOn "."
