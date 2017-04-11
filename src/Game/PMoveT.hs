{-# LANGUAGE TemplateHaskell #-}
module Game.PMoveT
    ( module Game.PMoveT
    ) where

import           Control.Lens     (makeLenses)
import qualified Data.Vector      as V
import           Linear           (V3(..))

import qualified Constants
import           Game.PMoveStateT (newPMoveStateT)
import           Game.TraceT      (newTraceT)
import           Game.UserCmdT    (newUserCmdT)
import           Types

makeLenses ''PMoveT

newPMoveT :: PMoveT
newPMoveT = PMoveT
    { _pmState         = newPMoveStateT
    , _pmCmd           = newUserCmdT
    , _pmSnapInitial   = False
    , _pmNumTouch      = 0
    , _pmTouchEnts     = V.replicate Constants.maxTouch Nothing
    , _pmViewAngles    = V3 0 0 0
    , _pmViewHeight    = 0
    , _pmMins          = V3 0 0 0
    , _pmMaxs          = V3 0 0 0
    , _pmGroundEntity  = Nothing
    , _pmWaterType     = 0
    , _pmWaterLevel    = 0
    , _pmTrace         = \_ _ _ _ -> return newTraceT
    , _pmPointContents = \_ -> return 0
    }
