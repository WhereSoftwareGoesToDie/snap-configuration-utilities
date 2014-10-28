{-# LANGUAGE OverloadedStrings #-}

module Snap.Utilities.Configuration.Extract (
    extractGroups,
    withValidGroup
) where

import Prelude hiding (lookup)
import Control.Applicative
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT
import Data.List (groupBy, intercalate, find, sortBy, lookup)
import Data.HashMap.Strict (toList)
import Data.Text (Text, isPrefixOf, splitOn, pack, unpack)

import Snap.Utilities.Configuration.Lookup
import Snap.Utilities.Configuration.Types
import Snap.Utilities.Configuration.Keys

------------------------------------------------------------------------------
extractGroups :: (ConfigPair -> Bool) -> CT.Config -> IO [[ConfigPair]]
extractGroups validator cfg = (groupBy authGroups . sortBy sortGroups . filter validator . toList) <$> C.getMap cfg
  where
    authGroups (k1, _) (k2, _) = (keyPre k1) == (keyPre k2)
    sortGroups (k1, _) (k2, _) = (keyPre k1) `compare` (keyPre k2)

withValidGroup :: String -> (String -> String -> [ConfigPair] -> a) -> [ConfigPair] -> a
withValidGroup groupKey transformer cfg =
    case fmap stringValue $ lookup (pack groupKey) cfg' of
        Just a -> transformer gn a cfg'
        _      -> error ("Not a valid " ++ groupKey ++ " in " ++ gn)
  where
    cfg' = map rebaseKey cfg
    gn   = groupName cfg
