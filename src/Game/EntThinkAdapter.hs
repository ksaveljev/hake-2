{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ImpredicativeTypes #-}
module Game.EntThinkAdapter where

import Control.Lens (makeLenses)
import qualified Data.ByteString as B

import Quake
import QuakeState

data EntThinkAdapter =
  EntThinkAdapter { _getId :: Quake B.ByteString
                  , _think :: QuakeLens EdictT -> Quake ()
                  }

makeLenses ''EntThinkAdapter
