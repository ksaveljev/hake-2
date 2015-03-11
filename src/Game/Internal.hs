module Game.Internal where

import Linear.V3 (V3)
import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as UV

import Quake
import Game.ClientPersistantT
import Game.ClientRespawnT
import Game.GItemT
import Game.LinkT
import Game.MonsterInfoT
import Game.MoveInfoT
import Game.PMoveStateT
import Game.PlayerStateT

data EdictT =
  EdictT { _eEntityState           :: EntityStateT
         , _eInUse                 :: Bool
         , _eLinkCount             :: Int
         , _eArea                  :: LinkT
         , _eNumClusters           :: Int
         , _eClusterNums           :: UV.Vector Int
         , _eHeadNode              :: Int
         , _eAreaNum               :: Int
         , _eAreaNum2              :: Int
         , _eSvFlags               :: Int
         , _eMins                  :: V3 Float
         , _eMaxs                  :: V3 Float
         , _eAbsMin                :: V3 Float
         , _eAbsMax                :: V3 Float
         , _eSize                  :: V3 Float
         , _eSolid                 :: Int
         , _eSlipMask              :: Int
         , _eMoveType              :: Int
         , _eFlags                 :: Int
         , _eModel                 :: B.ByteString
         , _eFreeTime              :: Float
         , _eMessage               :: B.ByteString
         , _eClassName             :: B.ByteString
         , _eSpawnFlags            :: Int
         , _eTimeStamp             :: Float
         , _eAngle                 :: Float
         , _eTarget                :: B.ByteString
         , _eTargetName            :: B.ByteString
         , _eKillTarget            :: B.ByteString
         , _eTeam                  :: B.ByteString
         , _ePathTarget            :: B.ByteString
         , _eDeathTarget           :: B.ByteString
         , _eCombatTarget          :: B.ByteString
         , _eTargetEnt             :: Maybe EdictT
         , _eSpeed                 :: Float
         , _eAccel                 :: Float
         , _eDecel                 :: Float
         , _eMoveDir               :: V3 Float
         , _ePos1                  :: V3 Float
         , _ePos2                  :: V3 Float
         , _eVelocity              :: V3 Float
         , _eAVelocity             :: V3 Float
         , _eMass                  :: Int
         , _eAirFinished           :: Float
         , _eGravity               :: Float
         , _eGoalEntity            :: Maybe EdictT
         , _eMoveTarget            :: Maybe EdictT
         , _eYawSpeed              :: Float
         , _eIdealYaw              :: Float
         , _eNextThink             :: Float
         , _ePrethink              :: Quake () -- TODO: ???
         , _eThink                 :: Quake () -- TODO: ???
         , _eBlocked               :: Quake () -- TODO: ???
         , _eTouch                 :: Quake () -- TODO: ???
         , _eUse                   :: Quake () -- TODO: ???
         , _ePain                  :: Quake () -- TODO: ???
         , _eDie                   :: Quake () -- TODO: ???
         , _eTouchDebounceTime     :: Float
         , _ePainDebounceTime      :: Float
         , _eDamageDebounceTime    :: Float
         , _eFlySoundDebounceTime  :: Float
         , _eLastMoveTime          :: Float
         , _eHealth                :: Int
         , _eMaxHealth             :: Int
         , _eGibHealth             :: Int
         , _eDeadFlag              :: Int
         , _eShowHostile           :: Int
         , _ePowerArmorTime        :: Float
         , _eMap                   :: Maybe B.ByteString
         , _eViewHeight            :: Int
         , _eTakeDamage            :: Int
         , _eDmg                   :: Int
         , _eRadiusDmg             :: Int
         , _eDmgRadius             :: Float
         , _eSounds                :: Int
         , _eCount                 :: Int
         , _eChain                 :: Maybe EdictT
         , _eEnemy                 :: Maybe EdictT
         , _eOldEnemy              :: Maybe EdictT
         , _eActivator             :: Maybe EdictT
         , _eGroundEntity          :: Maybe EdictT
         , _eGroundEntityLinkCount :: Int
         , _eTeamChain             :: Maybe EdictT
         , _eTeamMaster            :: Maybe EdictT
         , _eMyNoise               :: Maybe EdictT
         , _eMyNoise2              :: Maybe EdictT
         , _eNoiseIndex            :: Int
         , _eNoiseIndex2           :: Int
         , _eVolume                :: Float
         , _eAttenuation           :: Float
         , _eWait                  :: Float
         , _eDelay                 :: Float
         , _eRandom                :: Float
         , _eTeleportTime          :: Float
         , _eWaterType             :: Int
         , _eWaterLevel            :: Int
         , _eMoveOrigin            :: V3 Float
         , _eMoveAngles            :: V3 Float
         , _eLightLevel            :: Int
         , _eStyle                 :: Int
         , _eItem                  :: GItemT
         , _eMoveInfo              :: MoveInfoT
         , _eMonsterInfo           :: MonsterInfoT
         , _eClient                :: GClientT
         , _eOwner                 :: Maybe EdictT
         , _eIndex                 :: Int
         }

data EntityStateT =
  EntityStateT { _esNumber         :: Int
               , _esSurroundingEnt :: Maybe EdictT
               , _esOrigin         :: V3 Float
               , _esAngles         :: V3 Float
               , _esOldOrigin      :: V3 Float
               , _esModelIndex     :: Int
               , _esModelIndex2    :: Int
               , _esModelIndex3    :: Int
               , _esModelIndex4    :: Int
               , _esFrame          :: Int
               , _esSkinNum        :: Int
               , _esEffects        :: Int
               , _esRenderFx       :: Int
               , _esSolid          :: Int
               , _esSound          :: Int
               , _esEvent          :: Int
               }

data GClientT =
  GClientT { _gcPlayerState        :: PlayerStateT
           , _gcPing               :: Int
           , _gcPers               :: ClientPersistantT
           , _gcResp               :: ClientRespawnT
           , _gcOldPMove           :: PMoveStateT
           , _gcShowScores         :: Bool
           , _gcShowInventory      :: Bool
           , _gcShowHelp           :: Bool
           , _gcShowHelpIcon       :: Bool
           , _gcAmmoIndex          :: Int
           , _gcButtons            :: Int
           , _gcOldButtons         :: Int
           , _gcLatchedButtons     :: Int
           , _gcWeaponThunk        :: Bool
           , _gcNewWeapon          :: GItemT
           , _gcDamageArmor        :: Int
           , _gcDamagePArmor       :: Int
           , _gcDamageBlood        :: Int
           , _gcDamageKnockback    :: Int
           , _gcDamageFrom         :: V3 Float
           , _gcKillerYaw          :: Float
           , _gcWeaponState        :: Int
           , _gcKickAngles         :: V3 Float
           , _gcKickOrigin         :: V3 Float
           , _gcVDmgRoll           :: Float
           , _gcVDmgPitch          :: Float
           , _gcVDmgTime           :: Float
           , _gcFallTime           :: Float
           , _gcFallValue          :: Float
           , _gcDamageAlpha        :: Float
           , _gcBonusAlpha         :: Float
           , _gcDamageBlend        :: V3 Float
           , _gcVAngle             :: V3 Float
           , _gcBobTime            :: Float
           , _gcOldViewAngles      :: V3 Float
           , _gcOldVelocity        :: V3 Float
           , _gcNextDrownTime      :: Float
           , _gcOldWaterLevel      :: Int
           , _gcBreatherSound      :: Int
           , _gcMachinegunShots    :: Int
           , _gcAnimEnd            :: Int
           , _gcAnimPriority       :: Int
           , _gcAnimDuck           :: Bool
           , _gcAnimRun            :: Bool
           , _gcQuadFrameNum       :: Float
           , _gcInvincibleFrameNum :: Float
           , _gcBreatherFrameNum   :: Float
           , _gcEnviroFrameNum     :: Float
           , _gcGrenadeBlewUp      :: Bool
           , _gcGrenadeTime        :: Float
           , _gcSilencerShots      :: Int
           , _gcWeaponSound        :: Int
           , _gcPickupMsgTime      :: Float
           , _gcFloodLockTill      :: Float
           , _gcFloodWhen          :: UV.Vector Float
           , _gcFloodWhenHead      :: Int
           , _gcRespawnTime        :: Float
           , _gcChaseTarget        :: EdictT
           , _gcUpdateChase        :: Bool
           , _gcIndex              :: Int
           }
