{-# LANGUAGE TemplateHaskell #-}
module Game.PlayerTrailGlobals where

import Control.Lens (makeLenses)
import qualified Data.Vector as V

import Internal

makeLenses ''PlayerTrailGlobals

initialPlayerTrailGlobals :: PlayerTrailGlobals
initialPlayerTrailGlobals =
  PlayerTrailGlobals { _ptTrail       = V.empty
                     , _ptTrailHead   = 0
                     , _ptTrailActive = False
                     }
