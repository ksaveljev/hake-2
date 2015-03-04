module Game.PMove where
  
import Linear.V3 (V3)
import qualified Data.Vector.Unboxed as UV

import Game.Edict
import Game.PMoveState
import Game.UserCmd

data PMove = PMove { pMoveState         :: PMoveState
                   , pMoveCmd           :: UserCmd
                   , pMoveSnapInitial   :: Bool
                   , pMoveNumTouch      :: Int
                   , pMoveTouchEnts     :: UV.Vector Edict
                   , pMoveViewAngles    :: V3 Float
                   , pMoveViewHeight    :: Float
                   , pMoveMins          :: V3 Float
                   , pMoveMaxs          :: V3 Float
                   , pMoveGroundEntity  :: Edict
                   , pMoveWaterType     :: Int
                   , pMoveWaterLevel    :: Int
                   , pMoveTrace         :: IO () -- TODO: ???
                   , pMovePointContents :: IO () -- TODO: ???
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
