{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ImpredicativeTypes #-}
module Game.EntTouchAdapter where

import Control.Lens (makeLenses)
import qualified Data.ByteString as B

import Quake
import QuakeState
import Game.CSurfaceT

data EntTouchAdapter =
  EntTouchAdapter { _getId :: Quake B.ByteString
                  , _touch :: QuakeLens EdictT -> QuakeLens EdictT -> CPlaneT -> CSurfaceT -> Quake ()
                  }

makeLenses ''EntTouchAdapter
