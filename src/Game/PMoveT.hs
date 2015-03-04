{-# LANGUAGE TemplateHaskell #-}
module Game.PMoveT where
  
import Linear.V3 (V3)
import Control.Lens (makeLenses)
import qualified Data.Vector.Unboxed as UV

import Game.EdictT
import Game.PMoveStateT
import Game.UserCmdT

data PMoveT =
  PMoveT { _pmState         :: PMoveStateT
         , _pmCmd           :: UserCmdT
         , _pmSnapInitial   :: Bool
         , _pmNumTouch      :: Int
         , _pmTouchEnts     :: UV.Vector EdictT
         , _pmViewAngles    :: V3 Float
         , _pmViewHeight    :: Float
         , _pmMins          :: V3 Float
         , _pmMaxs          :: V3 Float
         , _pmGroundEntity  :: EdictT
         , _pmWaterType     :: Int
         , _pmWaterLevel    :: Int
         , _pmTrace         :: IO () -- TODO: ???
         , _pmPointContents :: IO () -- TODO: ???
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
         }

makeLenses ''PMoveT
