{-# LANGUAGE TemplateHaskell #-}
module Game.EdictT ( EdictT(..)
                   , module Game.EdictT
                   , module Game.EdictActionT
                   , module Game.EdictOtherT
                   , module Game.EdictTimingT
                   , module Game.EdictMinMaxT
                   , module Game.EdictInfoT
                   , module Game.EdictPhysicsT
                   , module Game.EdictStatusT
                   , module Game.GClientT
                   ) where

import Control.Lens (makeLenses)
import Linear (V3(..))
import qualified Data.Vector.Unboxed as UV

import Internal
import Game.EdictActionT
import Game.EdictOtherT
import Game.EdictTimingT
import Game.EdictMinMaxT
import Game.EdictInfoT
import Game.EdictPhysicsT
import Game.EdictStatusT
import Game.EntityStateT
import Game.GClientT
import Game.MonsterInfoT
import Game.MoveInfoT
import qualified Constants

makeLenses ''EdictT

newEdictT :: Int -> EdictT
newEdictT idx =
  EdictT { _eEntityState           = newEntityStateT { _esNumber = idx }
         , _eInUse                 = False
         , _eLinkCount             = 0
         , _eArea                  = 2 * Constants.areaNodes + idx -- index to svGlobals.svLinks
         , _eNumClusters           = 0
         , _eClusterNums           = UV.replicate Constants.maxEntClusters 0
         , _eHeadNode              = 0
         , _eAreaNum               = 0
         , _eAreaNum2              = 0
         , _eSvFlags               = 0
         , _eSolid                 = 0
         , _eClipMask              = 0
         , _eMoveType              = 0
         , _eFlags                 = 0
         , _eFreeTime              = 0
         , _eSpawnFlags            = 0
         , _eTimeStamp             = 0
         , _eEdictPhysics          = newEdictPhysicsT
         , _eTargetEnt             = Nothing
         , _eGoalEntity            = Nothing
         , _eMoveTarget            = Nothing
         , _eEdictAction           = newEdictActionT
         , _eEdictTiming           = newEdictTimingT
         , _eEdictStatus           = newEdictStatusT
         , _eSounds                = 0
         , _eCount                 = 0
         , _eGroundEntityLinkCount = 0
         , _eEdictOther            = newEdictOtherT
         , _eNoiseIndex            = 0
         , _eNoiseIndex2           = 0
         , _eVolume                = 0
         , _eAttenuation           = 0
         , _eWait                  = 0
         , _eDelay                 = 0
         , _eRandom                = 0
         , _eTeleportTime          = 0
         , _eWaterType             = 0
         , _eWaterLevel            = 0
         , _eMoveOrigin            = V3 0 0 0
         , _eMoveAngles            = V3 0 0 0
         , _eLightLevel            = 0
         , _eStyle                 = 0
         , _eItem                  = Nothing
         , _eMoveInfo              = newMoveInfoT
         , _eMonsterInfo           = newMonsterInfoT
         , _eClient                = Nothing
         , _eOwner                 = Nothing
         , _eIndex                 = idx
         , _eEdictInfo             = newEdictInfoT
         }
