module Game.Internal where

import Linear.V3 (V3)
import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as UV

import Game.ClientPersistantT
import Game.ClientRespawnT
import Game.GItemT
import Game.LinkT
import Game.MonsterInfoT
import Game.MoveInfoT
import Game.PMoveStateT
import Game.PlayerStateT

data EdictT =
  EdictT { eEntityState           :: EntityStateT
         , eInUse                 :: Bool
         , eLinkCount             :: Int
         , eArea                  :: LinkT
         , eNumClusters           :: Int
         , eClusterNums           :: UV.Vector Int
         , eHeadNode              :: Int
         , eAreaNum               :: Int
         , eAreaNum2              :: Int
         , eSvFlags               :: Int
         , eMins                  :: V3 Float
         , eMaxs                  :: V3 Float
         , eAbsMin                :: V3 Float
         , eAbsMax                :: V3 Float
         , eSize                  :: V3 Float
         , eSolid                 :: Int
         , eSlipMask              :: Int
         , eMoveType              :: Int
         , eFlags                 :: Int
         , eModel                 :: B.ByteString
         , eFreeTime              :: Float
         , eMessage               :: B.ByteString
         , eClassName             :: B.ByteString
         , eSpawnFlags            :: Int
         , eTimeStamp             :: Float
         , eAngle                 :: Float
         , eTarget                :: B.ByteString
         , eTargetName            :: B.ByteString
         , eKillTarget            :: B.ByteString
         , eTeam                  :: B.ByteString
         , ePathTarget            :: B.ByteString
         , eDeathTarget           :: B.ByteString
         , eCombatTarget          :: B.ByteString
         , eTargetEnt             :: Maybe EdictT
         , eSpeed                 :: Float
         , eAccel                 :: Float
         , eDecel                 :: Float
         , eMoveDir               :: V3 Float
         , ePos1                  :: V3 Float
         , ePos2                  :: V3 Float
         , eVelocity              :: V3 Float
         , eAVelocity             :: V3 Float
         , eMass                  :: Int
         , eAirFinished           :: Float
         , eGravity               :: Float
         , eGoalEntity            :: Maybe EdictT
         , eMoveTarget            :: Maybe EdictT
         , eYawSpeed              :: Float
         , eIdealYaw              :: Float
         , eNextThink             :: Float
         , ePrethink              :: IO () -- TODO: ???
         , eThink                 :: IO () -- TODO: ???
         , eBlocked               :: IO () -- TODO: ???
         , eTouch                 :: IO () -- TODO: ???
         , eUse                   :: IO () -- TODO: ???
         , ePain                  :: IO () -- TODO: ???
         , eDie                   :: IO () -- TODO: ???
         , eTouchDebounceTime     :: Float
         , ePainDebounceTime      :: Float
         , eDamageDebounceTime    :: Float
         , eFlySoundDebounceTime  :: Float
         , eLastMoveTime          :: Float
         , eHealth                :: Int
         , eMaxHealth             :: Int
         , eGibHealth             :: Int
         , eDeadFlag              :: Int
         , eShowHostile           :: Int
         , ePowerArmorTime        :: Float
         , eMap                   :: Maybe B.ByteString
         , eViewHeight            :: Int
         , eTakeDamage            :: Int
         , eDmg                   :: Int
         , eRadiusDmg             :: Int
         , eDmgRadius             :: Float
         , eSounds                :: Int
         , eCount                 :: Int
         , eChain                 :: Maybe EdictT
         , eEnemy                 :: Maybe EdictT
         , eOldEnemy              :: Maybe EdictT
         , eActivator             :: Maybe EdictT
         , eGroundEntity          :: Maybe EdictT
         , eGroundEntityLinkCount :: Int
         , eTeamChain             :: Maybe EdictT
         , eTeamMaster            :: Maybe EdictT
         , eMyNoise               :: Maybe EdictT
         , eMyNoise2              :: Maybe EdictT
         , eNoiseIndex            :: Int
         , eNoiseIndex2           :: Int
         , eVolume                :: Float
         , eAttenuation           :: Float
         , eWait                  :: Float
         , eDelay                 :: Float
         , eRandom                :: Float
         , eTeleportTime          :: Float
         , eWaterType             :: Int
         , eWaterLevel            :: Int
         , eMoveOrigin            :: V3 Float
         , eMoveAngles            :: V3 Float
         , eLightLevel            :: Int
         , eStyle                 :: Int
         , eItem                  :: GItemT
         , eMoveInfo              :: MoveInfoT
         , eMonsterInfo           :: MonsterInfoT
         , eClient                :: GClientT
         , eOwner                 :: Maybe EdictT
         , eIndex                 :: Int
         }

data EntityStateT =
  EntityStateT { esNumber         :: Int
               , esSurroundingEnt :: Maybe EdictT
               , esOrigin         :: V3 Float
               , esAngles         :: V3 Float
               , esOldOrigin      :: V3 Float
               , esModelIndex     :: Int
               , esModelIndex2    :: Int
               , esModelIndex3    :: Int
               , esModelIndex4    :: Int
               , esFrame          :: Int
               , esSkinNum        :: Int
               , esEffects        :: Int
               , esRenderFx       :: Int
               , esSolid          :: Int
               , esSound          :: Int
               , esEvent          :: Int
               }

data GClientT =
  GClientT { gcPlayerState        :: PlayerStateT
           , gcPing               :: Int
           , gcPers               :: ClientPersistantT
           , gcResp               :: ClientRespawnT
           , gcOldPMove           :: PMoveStateT
           , gcShowScores         :: Bool
           , gcShowInventory      :: Bool
           , gcShowHelp           :: Bool
           , gcShowHelpIcon       :: Bool
           , gcAmmoIndex          :: Int
           , gcButtons            :: Int
           , gcOldButtons         :: Int
           , gcLatchedButtons     :: Int
           , gcWeaponThunk        :: Bool
           , gcNewWeapon          :: GItemT
           , gcDamageArmor        :: Int
           , gcDamagePArmor       :: Int
           , gcDamageBlood        :: Int
           , gcDamageKnockback    :: Int
           , gcDamageFrom         :: V3 Float
           , gcKillerYaw          :: Float
           , gcWeaponState        :: Int
           , gcKickAngles         :: V3 Float
           , gcKickOrigin         :: V3 Float
           , gcVDmgRoll           :: Float
           , gcVDmgPitch          :: Float
           , gcVDmgTime           :: Float
           , gcFallTime           :: Float
           , gcFallValue          :: Float
           , gcDamageAlpha        :: Float
           , gcBonusAlpha         :: Float
           , gcDamageBlend        :: V3 Float
           , gcVAngle             :: V3 Float
           , gcBobTime            :: Float
           , gcOldViewAngles      :: V3 Float
           , gcOldVelocity        :: V3 Float
           , gcNextDrownTime      :: Float
           , gcOldWaterLevel      :: Int
           , gcBreatherSound      :: Int
           , gcMachinegunShots    :: Int
           , gcAnimEnd            :: Int
           , gcAnimPriority       :: Int
           , gcAnimDuck           :: Bool
           , gcAnimRun            :: Bool
           , gcQuadFrameNum       :: Float
           , gcInvincibleFrameNum :: Float
           , gcBreatherFrameNum   :: Float
           , gcEnviroFrameNum     :: Float
           , gcGrenadeBlewUp      :: Bool
           , gcGrenadeTime        :: Float
           , gcSilencerShots      :: Int
           , gcWeaponSound        :: Int
           , gcPickupMsgTime      :: Float
           , gcFloodLockTill      :: Float
           , gcFloodWhen          :: UV.Vector Float
           , gcFloodWhenHead      :: Int
           , gcRespawnTime        :: Float
           , gcChaseTarget        :: EdictT
           , gcUpdateChase        :: Bool
           , gcIndex              :: Int
           }
