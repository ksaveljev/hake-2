{-# LANGUAGE TemplateHaskell #-}
module Game.PMoveT ( PMoveT(..)
                   , module Game.PMoveT
                   , module Game.TraceT
                   , module Game.UserCmdT
                   ) where
  
import Control.Lens (makeLenses)
import Linear (V3(..))
import qualified Data.Vector.Unboxed as UV

import Internal
import Game.PMoveStateT
import Game.TraceT
import Game.UserCmdT
import qualified Constants

{-
TODO:
PMF_DUCKED = 1
PMF_JUMP_HELD = 2
PMF_ON_GROUND = 4
PMF_TIME_WATERJUMP = 8
PMF_TIME_LAND = 16
PMF_TIME_TELEPORT = 32
PMF_NO_PREDICTION = 64
-}

makeLenses ''PMoveT

newPMoveT :: PMoveT
newPMoveT =
  PMoveT { _pmState         = newPMoveStateT
         , _pmCmd           = newUserCmdT
         , _pmSnapInitial   = False
         , _pmNumTouch      = 0
         , _pmTouchEnts     = UV.replicate Constants.maxTouch 0 -- index to gameBaseGlobals.gbGEdicts
         , _pmViewAngles    = V3 0 0 0
         , _pmViewHeight    = 0
         , _pmMins          = V3 0 0 0
         , _pmMaxs          = V3 0 0 0
         , _pmGroundEntity  = Nothing
         , _pmWaterType     = 0
         , _pmWaterLevel    = 0
         , _pmTrace         = (\_ _ _ _ -> return Nothing)
         , _pmPointContents = (\_ -> 0)
         }
