{-# LANGUAGE TemplateHaskell #-}
module Game.PMoveT ( PMoveT(..)
                   , module Game.PMoveT
                   , module Game.TraceT
                   , module Game.UserCmdT
                   ) where
  
import Control.Lens (makeLenses)
import Linear (V3(..))
import qualified Data.Vector as V

import Internal
import Game.PMoveStateT
import Game.TraceT
import Game.UserCmdT
import qualified Constants

pmfDucked :: Int
pmfDucked = 1

pmfJumpHeld :: Int
pmfJumpHeld = 2

pmfOnGround :: Int
pmfOnGround = 4

pmfTimeWaterJump :: Int
pmfTimeWaterJump = 8

pmfTimeLand :: Int
pmfTimeLand = 16

pmfTimeTeleport :: Int
pmfTimeTeleport = 32

pmfNoPrediction :: Int
pmfNoPrediction = 64

makeLenses ''PMoveT

newPMoveT :: PMoveT
newPMoveT =
  PMoveT { _pmState         = newPMoveStateT
         , _pmCmd           = newUserCmdT
         , _pmSnapInitial   = False
         , _pmNumTouch      = 0
         , _pmTouchEnts     = V.replicate Constants.maxTouch (EdictReference (-1))
         , _pmViewAngles    = V3 0 0 0
         , _pmViewHeight    = 0
         , _pmMins          = V3 0 0 0
         , _pmMaxs          = V3 0 0 0
         , _pmGroundEntity  = Nothing
         , _pmWaterType     = 0
         , _pmWaterLevel    = 0
         , _pmTrace         = \_ _ _ _ -> return Nothing
         , _pmPointContents = \_ -> return 0
         }
