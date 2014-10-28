{-# LANGUAGE OverloadedStrings #-}

module Snap.Utilities.Configuration.Types (
	ConfigPair
) where

import qualified Data.Configurator.Types as CT
import Data.Text (Text)

------------------------------------------------------------------------------
-- | Look up a value.
type ConfigPair = (Text, CT.Value)
