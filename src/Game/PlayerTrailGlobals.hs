{-# LANGUAGE TemplateHaskell #-}
module Game.PlayerTrailGlobals
  ( module Game.PlayerTrailGlobals
  ) where

import           Types

import           Control.Lens (makeLenses)
import qualified Data.Vector as V

makeLenses ''PlayerTrailGlobals

initialPlayerTrailGlobals :: PlayerTrailGlobals
initialPlayerTrailGlobals =
  PlayerTrailGlobals { _ptTrail       = V.empty
                     , _ptTrailHead   = 0
                     , _ptTrailActive = False
                     }