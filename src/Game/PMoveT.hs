{-# LANGUAGE TemplateHaskell #-}
module Game.PMoveT where
  
import           Control.Lens     (makeLenses)
import           Data.Int         (Int8)
import qualified Data.Vector      as V
import           Linear           (V3(..))

import qualified Constants
import           Game.PMoveStateT
import           Game.TraceT
import           Game.UserCmdT
import           Types

pmfDucked :: Int8
pmfDucked = 1

pmfJumpHeld :: Int8
pmfJumpHeld = 2

pmfOnGround :: Int8
pmfOnGround = 4

pmfTimeWaterJump :: Int8
pmfTimeWaterJump = 8

pmfTimeLand :: Int8
pmfTimeLand = 16

pmfTimeTeleport :: Int8
pmfTimeTeleport = 32

pmfNoPrediction :: Int8
pmfNoPrediction = 64

makeLenses ''PMoveT

newPMoveT :: PMoveT
newPMoveT = PMoveT
    { _pmState         = newPMoveStateT
    , _pmCmd           = newUserCmdT
    , _pmSnapInitial   = False
    , _pmNumTouch      = 0
    , _pmTouchEnts     = V.replicate Constants.maxTouch (Ref (-1))
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
