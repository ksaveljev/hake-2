{-# LANGUAGE TemplateHaskell #-}
module QCommon.PMoveGlobals ( module QCommon.PMoveGlobals
                            , module Game.PMoveT
                            , module QCommon.PmlT
                            ) where

import Control.Lens (makeLenses)
import Linear.V3 (V3(..))
import qualified Data.Vector as V

import Types
import Game.PMoveT
import QCommon.PmlT
import qualified Constants

makeLenses ''PMoveGlobals

initialPMoveGlobals :: PMoveGlobals
initialPMoveGlobals =
  PMoveGlobals { _pmPM              = newPMoveT
               , _pmPML             = newPmlT
               , _pmPlanes          = V.replicate Constants.maxClipPlanes (V3 0 0 0)
               , _pmStopSpeed       = 100
               , _pmMaxSpeed        = 300
               , _pmDuckSpeed       = 100
               , _pmAccelerate      = 10
               , _pmAirAccelerate   = 0
               , _pmWaterAccelerate = 10
               , _pmFriction        = 6
               , _pmWaterFriction   = 1
               , _pmWaterSpeed      = 400
               }
