module Game.Internal where

import Linear.V3 (V3)
import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as UV

import Game.ClientPersistant
import Game.ClientRespawn
import Game.GItem
import Game.Link
import Game.MonsterInfo
import Game.MoveInfo
import Game.PMoveState
import Game.PlayerState

data Edict = Edict { edictEntityState           :: EntityState
                   , edictInUse                 :: Bool
                   , edictLinkCount             :: Int
                   , edictArea                  :: Link
                   , edictNumClusters           :: Int
                   , edictClusterNums           :: UV.Vector Int
                   , edictHeadNode              :: Int
                   , edictAreaNum               :: Int
                   , edictAreaNum2              :: Int
                   , edictSvFlags               :: Int
                   , edictMins                  :: V3 Float
                   , edictMaxs                  :: V3 Float
                   , edictAbsMin                :: V3 Float
                   , edictAbsMax                :: V3 Float
                   , edictSize                  :: V3 Float
                   , edictSolid                 :: Int
                   , edictSlipMask              :: Int
                   , edictMoveType              :: Int
                   , edictFlags                 :: Int
                   , edictModel                 :: B.ByteString
                   , edictFreeTime              :: Float
                   , edictMessage               :: B.ByteString
                   , edictClassName             :: B.ByteString
                   , edictSpawnFlags            :: Int
                   , edictTimeStamp             :: Float
                   , edictAngle                 :: Float
                   , edictTarget                :: B.ByteString
                   , edictTargetName            :: B.ByteString
                   , edictKillTarget            :: B.ByteString
                   , edictTeam                  :: B.ByteString
                   , edictPathTarget            :: B.ByteString
                   , edictDeathTarget           :: B.ByteString
                   , edictCombatTarget          :: B.ByteString
                   , edictTargetEnt             :: Maybe Edict
                   , edictSpeed                 :: Float
                   , edictAccel                 :: Float
                   , edictDecel                 :: Float
                   , edictMoveDir               :: V3 Float
                   , edictPos1                  :: V3 Float
                   , edictPos2                  :: V3 Float
                   , edictVelocity              :: V3 Float
                   , edictAVelocity             :: V3 Float
                   , edictMass                  :: Int
                   , edictAirFinished           :: Float
                   , edictGravity               :: Float
                   , edictGoalEntity            :: Maybe Edict
                   , edictMoveTarget            :: Maybe Edict
                   , edictYawSpeed              :: Float
                   , edictIdealYaw              :: Float
                   , edictNextThink             :: Float
                   , edictPrethink              :: IO () -- TODO: ???
                   , edictThink                 :: IO () -- TODO: ???
                   , edictBlocked               :: IO () -- TODO: ???
                   , edictTouch                 :: IO () -- TODO: ???
                   , edictUse                   :: IO () -- TODO: ???
                   , edictPain                  :: IO () -- TODO: ???
                   , edictDie                   :: IO () -- TODO: ???
                   , edictTouchDebounceTime     :: Float
                   , edictPainDebounceTime      :: Float
                   , edictDamageDebounceTime    :: Float
                   , edictFlySoundDebounceTime  :: Float
                   , edictLastMoveTime          :: Float
                   , edictHealth                :: Int
                   , edictMaxHealth             :: Int
                   , edictGibHealth             :: Int
                   , edictDeadFlag              :: Int
                   , edictShowHostile           :: Int
                   , edictPowerArmorTime        :: Float
                   , edictMap                   :: Maybe B.ByteString
                   , edictViewHeight            :: Int
                   , edictTakeDamage            :: Int
                   , edictDmg                   :: Int
                   , edictRadiusDmg             :: Int
                   , edictDmgRadius             :: Float
                   , edictSounds                :: Int
                   , edictCount                 :: Int
                   , edictChain                 :: Maybe Edict
                   , edictEnemy                 :: Maybe Edict
                   , edictOldEnemy              :: Maybe Edict
                   , edictActivator             :: Maybe Edict
                   , edictGroundEntity          :: Maybe Edict
                   , edictGroundEntityLinkCount :: Int
                   , edictTeamChain             :: Maybe Edict
                   , edictTeamMaster            :: Maybe Edict
                   , edictMyNoise               :: Maybe Edict
                   , edictMyNoise2              :: Maybe Edict
                   , edictNoiseIndex            :: Int
                   , edictNoiseIndex2           :: Int
                   , edictVolume                :: Float
                   , edictAttenuation           :: Float
                   , edictWait                  :: Float
                   , edictDelay                 :: Float
                   , edictRandom                :: Float
                   , edictTeleportTime          :: Float
                   , edictWaterType             :: Int
                   , edictWaterLevel            :: Int
                   , edictMoveOrigin            :: V3 Float
                   , edictMoveAngles            :: V3 Float
                   , edictLightLevel            :: Int
                   , edictStyle                 :: Int
                   , edictItem                  :: GItem
                   , edictMoveInfo              :: MoveInfo
                   , edictMonsterInfo           :: MonsterInfo
                   , edictClient                :: GClient
                   , edictOwner                 :: Maybe Edict
                   , edictIndex                 :: Int
                   }

data EntityState = EntityState { entityStateNumber         :: Int
                               , entityStateSurroundingEnt :: Maybe Edict
                               , entityStateOrigin         :: V3 Float
                               , entityStateAngles         :: V3 Float
                               , entityStateOldOrigin      :: V3 Float
                               , entityStateModelIndex     :: Int
                               , entityStateModelIndex2    :: Int
                               , entityStateModelIndex3    :: Int
                               , entityStateModelIndex4    :: Int
                               , entityStateFrame          :: Int
                               , entityStateSkinNum        :: Int
                               , entityStateEffects        :: Int
                               , entityStateRenderFx       :: Int
                               , entityStateSolid          :: Int
                               , entityStateSound          :: Int
                               , entityStateEvent          :: Int
                               }

data GClient = GClient { gClientPlayerState        :: PlayerState
                       , gClientPing               :: Int
                       , gClientPers               :: ClientPersistant
                       , gClientResp               :: ClientRespawn
                       , gClientOldPMove           :: PMoveState
                       , gClientShowScores         :: Bool
                       , gClientShowInventory      :: Bool
                       , gClientShowHelp           :: Bool
                       , gClientShowHelpIcon       :: Bool
                       , gClientAmmoIndex          :: Int
                       , gClientButtons            :: Int
                       , gClientOldButtons         :: Int
                       , gClientLatchedButtons     :: Int
                       , gClientWeaponThunk        :: Bool
                       , gClientNewWeapon          :: GItem
                       , gClientDamageArmor        :: Int
                       , gClientDamagePArmor       :: Int
                       , gClientDamageBlood        :: Int
                       , gClientDamageKnockback    :: Int
                       , gClientDamageFrom         :: V3 Float
                       , gClientKillerYaw          :: Float
                       , gClientWeaponState        :: Int
                       , gClientKickAngles         :: V3 Float
                       , gClientKickOrigin         :: V3 Float
                       , gClientVDmgRoll           :: Float
                       , gClientVDmgPitch          :: Float
                       , gClientVDmgTime           :: Float
                       , gClientFallTime           :: Float
                       , gClientFallValue          :: Float
                       , gClientDamageAlpha        :: Float
                       , gClientBonusAlpha         :: Float
                       , gClientDamageBlend        :: V3 Float
                       , gClientVAngle             :: V3 Float
                       , gClientBobTime            :: Float
                       , gClientOldViewAngles      :: V3 Float
                       , gClientOldVelocity        :: V3 Float
                       , gClientNextDrownTime      :: Float
                       , gClientOldWaterLevel      :: Int
                       , gClientBreatherSound      :: Int
                       , gClientMachinegunShots    :: Int
                       , gClientAnimEnd            :: Int
                       , gClientAnimPriority       :: Int
                       , gClientAnimDuck           :: Bool
                       , gClientAnimRun            :: Bool
                       , gClientQuadFrameNum       :: Float
                       , gClientInvincibleFrameNum :: Float
                       , gClientBreatherFrameNum   :: Float
                       , gClientEnviroFrameNum     :: Float
                       , gClientGrenadeBlewUp      :: Bool
                       , gClientGrenadeTime        :: Float
                       , gClientSilencerShots      :: Int
                       , gClientWeaponSound        :: Int
                       , gClientPickupMsgTime      :: Float
                       , gClientFloodLockTill      :: Float
                       , gClientFloodWhen          :: UV.Vector Float
                       , gClientFloodWhenHead      :: Int
                       , gClientRespawnTime        :: Float
                       , gClientChaseTarget        :: Edict
                       , gClientUpdateChase        :: Bool
                       , gClientIndex              :: Int
                       }
