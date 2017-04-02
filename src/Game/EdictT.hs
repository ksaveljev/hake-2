{-# LANGUAGE TemplateHaskell #-}
module Game.EdictT where

import           Control.Lens        (makeLenses)
import qualified Data.Vector.Unboxed as UV
import           Linear              (V3(..))

import qualified Constants
import           Game.EntityStateT
import           Game.GClientT
import           Game.MonsterInfoT
import           Game.MoveInfoT
import           Types

makeLenses ''EdictT

newEdictT :: Int -> EdictT
newEdictT idx = EdictT
    { _eEntityState           = newEntityStateT (Just (Ref idx))
    , _eInUse                 = False
    , _eClassName             = ""
    , _eLinkCount             = 0
    , _eArea                  = Ref (2 * Constants.areaNodes + idx)
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
    , _eAngle                 = 0
    , _eSpeed                 = 0
    , _eAccel                 = 0
    , _eDecel                 = 0
    , _eMoveDir               = V3 0 0 0
    , _ePos1                  = V3 0 0 0
    , _ePos2                  = V3 0 0 0
    , _eVelocity              = V3 0 0 0
    , _eAVelocity             = V3 0 0 0
    , _eMass                  = 0
    , _eAirFinished           = 0
    , _eGravity               = 0
    , _eYawSpeed              = 0
    , _eIdealYaw              = 0
    , _eTargetEnt             = Nothing
    , _eGoalEntity            = Nothing
    , _eMoveTarget            = Nothing
    , _eNextThink             = 0
    , _ePrethink              = Nothing
    , _eThink                 = Nothing
    , _eBlocked               = Nothing
    , _eTouch                 = Nothing
    , _eUse                   = Nothing
    , _ePain                  = Nothing
    , _eDie                   = Nothing
    , _eTouchDebounceTime     = 0
    , _ePainDebounceTime      = 0
    , _eDamageDebounceTime    = 0
    , _eFlySoundDebounceTime  = 0
    , _eLastMoveTime          = 0
    , _eHealth                = 0
    , _eMaxHealth             = 0
    , _eGibHealth             = 0
    , _eDeadFlag              = 0
    , _eShowHostile           = 0
    , _ePowerArmorTime        = 0
    , _eViewHeight            = 0
    , _eTakeDamage            = 0
    , _eDmg                   = 0
    , _eRadiusDmg             = 0
    , _eDmgRadius             = 0
    , _eSounds                = 0
    , _eCount                 = 0
    , _eGroundEntityLinkCount = 0
    , _eChain                 = Nothing
    , _eEnemy                 = Nothing
    , _eOldEnemy              = Nothing
    , _eActivator             = Nothing
    , _eGroundEntity          = Nothing
    , _eTeamChain             = Nothing
    , _eTeamMaster            = Nothing
    , _eMyNoise               = Nothing
    , _eMyNoise2              = Nothing
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
    , _eIndex                 = if idx == Constants.maxEdicts then -1 else idx
    , _eiModel                = Nothing
    , _eMessage               = Nothing
    , _eTarget                = Nothing
    , _eTargetName            = Nothing
    , _eKillTarget            = Nothing
    , _eTeam                  = Nothing
    , _ePathTarget            = Nothing
    , _eDeathTarget           = Nothing
    , _eCombatTarget          = Nothing
    , _eMap                   = Nothing
    , _eMins                  = V3 0 0 0
    , _eMaxs                  = V3 0 0 0
    , _eAbsMin                = V3 0 0 0
    , _eAbsMax                = V3 0 0 0
    , _eSize                  = V3 0 0 0
    }
