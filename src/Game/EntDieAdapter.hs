{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ImpredicativeTypes #-}
module Game.EntDieAdapter where

import Control.Lens (makeLenses)
import Linear (V3)
import qualified Data.ByteString as B

import Quake
import QuakeState

data EntDieAdapter =
  EntDieAdapter { _getId :: Quake B.ByteString
                , _die   :: QuakeLens EdictT -> QuakeLens EdictT -> QuakeLens EdictT -> Int -> V3 Float -> Quake ()
                }

makeLenses ''EntDieAdapter
