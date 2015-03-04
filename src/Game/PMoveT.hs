module Game.PMoveT where
  
import Linear.V3 (V3)
import qualified Data.Vector.Unboxed as UV

import Game.EdictT
import Game.PMoveStateT
import Game.UserCmdT

data PMoveT =
  PMoveT { pmState         :: PMoveStateT
         , pmCmd           :: UserCmdT
         , pmSnapInitial   :: Bool
         , pmNumTouch      :: Int
         , pmTouchEnts     :: UV.Vector EdictT
         , pmViewAngles    :: V3 Float
         , pmViewHeight    :: Float
         , pmMins          :: V3 Float
         , pmMaxs          :: V3 Float
         , pmGroundEntity  :: EdictT
         , pmWaterType     :: Int
         , pmWaterLevel    :: Int
         , pmTrace         :: IO () -- TODO: ???
         , pmPointContents :: IO () -- TODO: ???
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
