{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Internal where

import Data.Word (Word8)
import Linear.V3 (V3)
import Data.Sequence (Seq)
import Control.Applicative
import Control.Monad.State.Strict
import Control.Monad.Except
import Control.Lens (Lens)
import System.IO (Handle)
import System.Random (StdGen)
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import Client.ClientStaticT
import Game.ClientPersistantT
import Game.ClientRespawnT
import Game.CmdAliasT
import Game.CModelT
import Game.CVarT
import Game.GItemT
import Game.LinkT
import Game.MonsterInfoT
import Game.MoveInfoT
import Game.PMoveStateT
import Game.PlayerStateT
import Game.UserCmdT
import Server.ChallengeT
import Server.ClientFrameT
import QCommon.FileLinkT
import QCommon.NetAdrT
import QCommon.NetChanT
import QCommon.SearchPathT
import QCommon.SizeBufT

newtype Quake a = Quake (StateT QuakeState (ExceptT B.ByteString IO) a)
                    deriving (Functor, Applicative, Monad, MonadIO, MonadError B.ByteString, MonadState QuakeState)

type XCommandT = Quake ()

data QuakeState =
  QuakeState { _globals           :: Globals
             , _comGlobals        :: ComGlobals
             , _cmdGlobals        :: CmdGlobals
             , _keyGlobals        :: KeyGlobals
             , _fsGlobals         :: FSGlobals
             , _netChannelGlobals :: NetChannelGlobals
             , _svGlobals         :: SVGlobals
             }

data Globals =
  Globals { _curtime            :: Int
          , _cmdWait            :: Bool

          , _aliasCount         :: Int
          , _cTraces            :: Int
          , _cBrushTraces       :: Int
          , _cPointContents     :: Int
          , _serverState        :: Int

          , _netMessage         :: SizeBufT
          , _netMessageBuffer   :: B.ByteString
          , _cmdText            :: SizeBufT
          , _cmdTextBuf         :: B.ByteString
          , _cmdAlias           :: Seq CmdAliasT

          , _timeBeforeGame     :: Int
          , _timeAfterGame      :: Int
          , _timeBeforeRef      :: Int
          , _timeAfterRef       :: Int

          , _logStatsFile       :: Maybe Handle

          , _cls                :: ClientStaticT

          , _userInfoModified   :: Bool

          , _cvarVars           :: M.Map B.ByteString CVarT

          , _keyBindings        :: V.Vector (Maybe B.ByteString)
          , _keyDown            :: UV.Vector Bool
          , _chatTeam           :: Bool
          , _chatBuffer         :: B.ByteString
          , _keyLines           :: V.Vector B.ByteString
          , _keyLinePos         :: Int
          , _editLine           :: Int

          , _rnd                :: StdGen
          }

data ComGlobals =
  ComGlobals { _cgComArgc     :: Int
             , _cgComArgv     :: V.Vector B.ByteString
             , _cgRecursive   :: Bool
             , _cgMsg         :: B.ByteString
             , _debugContext  :: B.ByteString
             , _debugContext2 :: B.ByteString
             }

data CmdGlobals =
  CmdGlobals { _cgCmdFunctions :: Seq CmdFunctionT
             , _cgCmdArgc      :: Int
             , _cgCmdArgv      :: V.Vector B.ByteString
             , _cgCmdArgs      :: B.ByteString
             }

data KeyGlobals =
  KeyGlobals { _anyKeyDown  :: Int
             , _keyWaiting  :: Int
             , _historyLine :: Int
             , _shiftDown   :: Bool
             , _keyRepeats  :: UV.Vector Int
             , _menuBound   :: UV.Vector Bool
             , _consoleKeys :: UV.Vector Bool
             , _keyNames    :: V.Vector (Maybe B.ByteString)
             }

data FSGlobals =
  FSGlobals { _fsGameDir         :: B.ByteString
            , _fsUserDir         :: B.ByteString
            , _fsLinks           :: Seq FileLinkT
            , _fsSearchPaths     :: [SearchPathT]
            , _fsBaseSearchPaths :: [SearchPathT]
            , _fsFileFromPak     :: Int
            }

data NetChannelGlobals =
  NetChannelGlobals {
                    }

data SVGlobals =
  SVGlobals { _svMasterAdr            :: V.Vector NetAdrT
            , _svClient               :: ClientT
            , _svServer               :: ServerT
            , _svServerStatic         :: ServerStaticT
            , _svPlayer               :: EdictT
            }

data CmdFunctionT =
  CmdFunctionT { _cfName     :: B.ByteString
               , _cfFunction :: Maybe XCommandT
               }

type QuakeLens a = Lens QuakeState QuakeState a a

data EdictActionT =
  EdictActionT { _eaNextThink :: Float
               , _eaPrethink  :: Quake () -- TODO: ???
               , _eaThink     :: Quake () -- TODO: ???
               , _eaBlocked   :: Quake () -- TODO: ???
               , _eaTouch     :: Quake () -- TODO: ???
               , _eaUse       :: Quake () -- TODO: ???
               , _eaPain      :: Quake () -- TODO: ???
               , _eaDie       :: Quake () -- TODO: ???
               }

data EdictOtherT =
  EdictOtherT { _eoChain        :: Maybe EdictT
              , _eoEnemy        :: Maybe EdictT
              , _eoOldEnemy     :: Maybe EdictT
              , _eoActivator    :: Maybe EdictT
              , _eoGroundEntity :: Maybe EdictT
              , _eoTeamChain    :: Maybe EdictT
              , _eoTeamMaster   :: Maybe EdictT
              , _eoMyNoise      :: Maybe EdictT
              , _eoMyNoise2     :: Maybe EdictT
              }

data EdictTimingT =
  EdictTimingT { _etTouchDebounceTime    :: Float
               , _etPainDebounceTime     :: Float
               , _etDamageDebounceTime   :: Float
               , _etFlySoundDebounceTime :: Float
               , _etLastMoveTime         :: Float
               }

data EdictMinMaxT =
  EdictMinMaxT { _eMins   :: V3 Float
               , _eMaxs   :: V3 Float
               , _eAbsMin :: V3 Float
               , _eAbsMax :: V3 Float
               , _eSize   :: V3 Float
               }

data EdictInfoT =
  EdictInfoT { _esModel        :: B.ByteString
             , _esMessage      :: B.ByteString
             , _esClassName    :: B.ByteString
             , _esTarget       :: B.ByteString
             , _esTargetName   :: B.ByteString
             , _esKillTarget   :: B.ByteString
             , _esTeam         :: B.ByteString
             , _esPathTarget   :: B.ByteString
             , _esDeathTarget  :: B.ByteString
             , _esCombatTarget :: B.ByteString
             , _esMap          :: B.ByteString
             }

data EdictPhysicsT =
  EdictPhysicsT { _eAngle       :: Float
                , _eSpeed       :: Float
                , _eAccel       :: Float
                , _eDecel       :: Float
                , _eMoveDir     :: V3 Float
                , _ePos1        :: V3 Float
                , _ePos2        :: V3 Float
                , _eVelocity    :: V3 Float
                , _eAVelocity   :: V3 Float
                , _eMass        :: Int
                , _eAirFinished :: Float
                , _eGravity     :: Float
                , _eYawSpeed    :: Float
                , _eIdealYaw    :: Float
                }

data EdictStatusT =
  EdictStatusT { _eHealth         :: Int
               , _eMaxHealth      :: Int
               , _eGibHealth      :: Int
               , _eDeadFlag       :: Int
               , _eShowHostile    :: Int
               , _ePowerArmorTime :: Float
               , _eViewHeight     :: Int
               , _eTakeDamage     :: Int
               , _eDmg            :: Int
               , _eRadiusDmg      :: Int
               , _eDmgRadius      :: Float
               }

-- had to split EdictT into smaller EdictXXX types in order
-- for makeLenses not to generate A LOT of code which eats up
-- A LOT of memory
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
         , _eSolid                 :: Int
         , _eSlipMask              :: Int
         , _eMoveType              :: Int
         , _eFlags                 :: Int
         , _eFreeTime              :: Float
         , _eSpawnFlags            :: Int
         , _eTimeStamp             :: Float
         , _eEdictPhysics          :: EdictPhysicsT
         , _eTargetEnt             :: Maybe EdictT
         , _eGoalEntity            :: Maybe EdictT
         , _eMoveTarget            :: Maybe EdictT
         , _eEdictAction           :: EdictActionT
         , _eEdictTiming           :: EdictTimingT
         , _eEdictStatus           :: EdictStatusT
         , _eSounds                :: Int
         , _eCount                 :: Int
         , _eGroundEntityLinkCount :: Int
         , _eEdictOther            :: EdictOtherT
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
         , _eEdictInfo             :: EdictInfoT
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

data ClientT =
  ClientT { _cState         :: Int
          , _cUserInfo      :: B.ByteString
          , _cLastFrame     :: Int
          , _cLastCmd       :: UserCmdT
          , _cCommandMsec   :: Int
          , _cFrameLatency  :: UV.Vector Int
          , _cPing          :: Int
          , _cMessageSize   :: UV.Vector Int
          , _cRate          :: Int
          , _cSurpressCount :: Int
          , _cEdict         :: EdictT
          , _cName          :: B.ByteString
          , _cMessageLevel  :: Int
          , _cDatagram      :: SizeBufT
          , _cDatagramBuf   :: B.ByteString
          , _cFrames        :: V.Vector ClientFrameT
          , _cDownload      :: B.ByteString
          , _cDownloadSize  :: Int
          , _cDownloadCount :: Int
          , _cLastMessage   :: Int
          , _cLastConnect   :: Int
          , _cChallenge     :: Int
          , _cNetChan       :: NetChanT
          , _cServerIndex   :: Int
          }

data ServerStaticT =
  ServerStaticT { _ssInitialized        :: Bool
                , _ssRealTime           :: Int
                , _ssMapCmd             :: B.ByteString
                , _ssSpawnCount         :: Int
                , _ssClients            :: V.Vector ClientT
                , _ssNumClientEntities  :: Int
                , _ssNextClientEntities :: Int
                , _ssClientEntities     :: V.Vector EntityStateT
                , _ssLastHeartbeat      :: Int
                , _ssChallenges         :: V.Vector ChallengeT
                , _ssDemoFile           :: B.ByteString
                , _ssDemoMulticast      :: SizeBufT
                , _ssDemoMulticastBuf   :: UV.Vector Word8
                }

data ServerT =
  ServerT { _sState         :: Int
          , _sAttractLoop   :: Bool
          , _sLoadGame      :: Bool
          , _sTime          :: Int
          , _sFrameNum      :: Int
          , _sName          :: B.ByteString
          , _sModels        :: V.Vector CModelT
          , _sConfigStrings :: V.Vector B.ByteString
          , _sBaselines     :: V.Vector EntityStateT
          , _sMulticast     :: SizeBufT
          , _sMulticastBuf  :: B.ByteString
          , _sDemoFile      :: Maybe Handle
          , _sTimeDemo      :: Int
          }
