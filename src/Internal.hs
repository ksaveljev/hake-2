{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Internal where

import Linear (V3, V4)
import Data.Int (Int16)
import Data.Word (Word8)
import Data.Sequence (Seq)
import Control.Applicative
import Control.Monad.State.Strict
import Control.Monad.Except
import Control.Lens (Lens')
import Network.Socket (Socket)
import System.IO (Handle)
import System.Random (StdGen)
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV


import Client.ClientStaticT
import Client.ConsoleT
import Client.DirtyT
import Client.DLightT
import Client.FrameT
import Client.LightStyleT
import Game.CmdAliasT
import Game.CModelT
import Game.CPlaneT
import Game.CSurfaceT
import Game.CVarT
import Game.GItemArmorT
import Game.LinkT
import Game.MMoveT
import Game.MoveInfoT
import Game.PMoveStateT
import Game.PlayerStateT
import Game.SpawnTempT
import Game.UserCmdT
import QCommon.FileLinkT
import QCommon.NetAdrT
import QCommon.NetChanT
import QCommon.PmlT
import QCommon.SearchPathT
import QCommon.SizeBufT
import Render.MEdgeT
import Render.MModelT
import Render.MNodeT
import Render.MVertexT
import Server.ChallengeT
import Server.ClientFrameT
import Sound.SfxT
import Sys.LoopbackT

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
             , _gameBaseGlobals   :: GameBaseGlobals
             , _pMoveGlobals      :: PMoveGlobals
             , _scrGlobals        :: SCRGlobals
             , _netGlobals        :: NETGlobals
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
          , _cl                 :: ClientStateT

          , _userInfoModified   :: Bool

          , _cvarVars           :: M.Map B.ByteString CVarT
          , _con                :: ConsoleT
          , _re                 :: RefExportT

          , _keyBindings        :: V.Vector (Maybe B.ByteString)
          , _keyDown            :: UV.Vector Bool
          , _chatTeam           :: Bool
          , _chatBuffer         :: B.ByteString
          , _keyLines           :: V.Vector B.ByteString
          , _keyLinePos         :: Int
          , _editLine           :: Int

          , _vec3Origin         :: V3 Float

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
            , _svFirstMap             :: B.ByteString
            , _svMsgBuf               :: B.ByteString
            }

data CmdFunctionT =
  CmdFunctionT { _cfName     :: B.ByteString
               , _cfFunction :: Maybe XCommandT
               }

data EdictActionT =
  EdictActionT { _eaNextThink :: Float
               , _eaPrethink  :: Quake ()
               , _eaThink     :: Quake ()
               , _eaBlocked   :: Quake ()
               , _eaTouch     :: Quake ()
               , _eaUse       :: Quake ()
               , _eaPain      :: Quake ()
               , _eaDie       :: Quake ()
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
                , _ssDemoFile           :: Maybe Handle
                , _ssDemoMulticast      :: SizeBufT
                , _ssDemoMulticastBuf   :: B.ByteString
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

data GameLocalsT =
  GameLocalsT { _glHelpMessage1 :: B.ByteString
              , _glHelpMessage2 :: B.ByteString
              , _glHelpChanged  :: Int
              , _glClients      :: V.Vector GClientT
              , _glSpawnPoint   :: B.ByteString
              , _glMaxClients   :: Int
              , _glMaxEntities  :: Int
              , _glServerFlags  :: Int
              , _glNumItems     :: Int
              , _glAutosaved    :: Bool
              }

data LevelLocalsT =
  LevelLocalsT { _llFrameNum             :: Int
               , _llTime                 :: Float
               , _llLevelName            :: B.ByteString
               , _llMapName              :: B.ByteString
               , _llNextMap              :: B.ByteString
               , _llIntermissionTime     :: Float
               , _llChangeMap            :: B.ByteString
               , _llExitIntermission     :: Bool
               , _llIntermissionOrigin   :: V3 Float
               , _llIntermissionAngle    :: V3 Float
               , _llSightClient          :: EdictT
               , _llSightEntity          :: EdictT
               , _llSightEntityFrameNum  :: Int
               , _llSoundEntity          :: EdictT
               , _llSoundEntityFrameNum  :: Int
               , _llSound2Entity         :: EdictT
               , _llSound2EntityFrameNum :: Int
               , _llPicHealth            :: Int
               , _llTotalSecrets         :: Int
               , _llFoundSecrets         :: Int
               , _llTotalGoals           :: Int
               , _llFoundGoals           :: Int
               , _llTotalMonsters        :: Int
               , _llKilledMonsters       :: Int
               , _llCurrentEntity        :: EdictT
               , _llBodyQue              :: Int
               , _llPowerCubes           :: Int
               }

data GameImportT =
  GameImportT { _giBprintf            :: Int -> B.ByteString -> Quake ()
              , _giDprintf            :: B.ByteString -> Quake ()
              , _giCprintf            :: EdictT -> Int -> B.ByteString -> Quake ()
              , _giCenterPrintf       :: EdictT -> B.ByteString -> Quake ()
              , _giSound              :: EdictT -> Int -> Int -> Float -> Float -> Float -> Quake ()
              , _giPositionedSound    :: V3 Float -> EdictT -> Int -> Int -> Float -> Float -> Float -> Quake ()
              , _giConfigString       :: Int -> B.ByteString -> Quake ()
              , _giError              :: B.ByteString -> Quake ()
              , _giError2             :: Int -> B.ByteString -> Quake ()
              , _giModelIndex         :: B.ByteString -> Quake Int
              , _giSoundIndex         :: B.ByteString -> Quake Int
              , _giImageIndex         :: B.ByteString -> Quake Int
              , _giSetModel           :: EdictT -> B.ByteString -> Quake ()
              , _giTrace              :: V3 Float -> V3 Float -> V3 Float -> V3 Float -> EdictT -> Int -> Quake TraceT
              --, pmove_t.PointContentsAdapter -- TODO: ???
              , _giInPHS              :: V3 Float -> V3 Float -> Quake Bool
              , _giSetAreaPortalState :: Int -> Bool -> Quake ()
              , _giAreasConnected     :: Int -> Int -> Quake Bool
              , _giLinkEntity         :: EdictT -> Quake ()
              , _giUnlinkEntity       :: EdictT -> Quake ()
              , _giBoxEdicts          :: V3 Float -> V3 Float -> V.Vector EdictT -> Int -> Int -> Quake Int
              , _giPMove              :: PMoveT -> Quake ()
              , _giMulticast          :: V3 Float -> Int -> Quake ()
              , _giUnicast            :: EdictT -> Bool -> Quake ()
              , _giWriteByte          :: Int -> Quake ()
              , _giWriteShort         :: Int -> Quake ()
              , _giWriteString        :: B.ByteString -> Quake ()
              , _giWritePosition      :: V3 Float -> Quake ()
              , _giWriteDir           :: V3 Float -> Quake ()
              , _giCvar               :: B.ByteString -> B.ByteString -> Int -> Quake (Maybe CVarT)
              , _giCvarSet            :: B.ByteString -> B.ByteString -> Quake CVarT
              , _giCvarForceset       :: B.ByteString -> B.ByteString -> Quake CVarT
              , _giArgc               :: Quake Int
              , _giArgv               :: Int -> Quake B.ByteString
              , _giArgs               :: Quake B.ByteString
              , _giAddCommandString   :: B.ByteString -> Quake ()
              }

data TraceT =
  TraceT { _tAllSolid   :: Bool
         , _tStartSolid :: Bool
         , _tFraction   :: Float
         , _tEndPos     :: V3 Float
         , _tPlane      :: CPlaneT
         , _tSurface    :: CSurfaceT
         , _tContents   :: Int
         , _tEnt        :: EdictT
         }

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
         , _pmTrace         :: Quake ()
         , _pmPointContents :: Quake ()
         }

data GameBaseGlobals =
  GameBaseGlobals { _gbDummyPlane        :: CPlaneT
                  , _gbGame              :: GameLocalsT
                  , _gbLevel             :: LevelLocalsT
                  , _gbGameImport        :: GameImportT
                  , _gbSpawnTemp         :: SpawnTempT
                  , _gbSmMeatIndex       :: Int
                  , _gbSndFry            :: Int
                  , _gbMeansOfDeath      :: Int
                  , _gbNumEdicts         :: Int
                  , _gbGEdicts           :: V.Vector EdictT
                  , _gbItemList          :: Seq GItemT
                  }

data PMoveGlobals =
  PMoveGlobals { _pmPM              :: PMoveT
               , _pmPML             :: PmlT
               , _pmPlanes          :: V.Vector (V3 Float)
               , _pmStopSpeed       :: Float
               , _pmMaxSpeed        :: Float
               , _pmDuckSpeed       :: Float
               , _pmAccelerate      :: Float
               , _pmAirAccelerate   :: Float
               , _pmWaterAccelerate :: Float
               , _pmFriction        :: Float
               , _pmWaterFriction   :: Float
               , _pmWaterSpeed      :: Float
               }

data MonsterInfoT =
  MonsterInfoT { _miCurrentMove     :: MMoveT
               , _miAIFlags         :: Int
               , _miNextFrame       :: Int
               , _miScale           :: Float
               , _miStand           :: Quake ()
               , _miIdle            :: Quake ()
               , _miSearch          :: Quake ()
               , _miWalk            :: Quake ()
               , _miRun             :: Quake ()
               , _miDodge           :: Quake ()
               , _miAttack          :: Quake ()
               , _miMelee           :: Quake ()
               , _miSight           :: Quake ()
               , _miCheckAttack     :: Quake ()
               , _miPauseTime       :: Float
               , _miAttackFinished  :: Float
               , _miSavedGoal       :: V3 Float
               , _miSearchTime      :: Float
               , _miTrailTime       :: Float
               , _miLastSighting    :: V3 Float
               , _miAttackState     :: Int
               , _miLefty           :: Int
               , _miIdleTime        :: Float
               , _miLinkCount       :: Int
               , _miPowerArmorType  :: Int
               , _miPowerArmorPower :: Int
               }

data ClientStateT =
  ClientStateT { _csTimeOutCount           :: Int
               , _csTimeDemoFrames         :: Int
               , _csTimeDemoStart          :: Int
               , _csRefreshPrepped         :: Bool
               , _csSoundPrepped           :: Bool
               , _csForceRefDef            :: Bool
               , _csParseEntities          :: Int
               , _csCmd                    :: UserCmdT
               , _csCmds                   :: V.Vector UserCmdT
               , _csCmdTime                :: UV.Vector Int
               , _csPredictedOrigins       :: UV.Vector (V3 Int16)
               , _csPredictedStep          :: Float
               , _csPredictedStepTime      :: Int
               , _csPredictedOrigin        :: V3 Float
               , _csPredictedAngles        :: V3 Float
               , _csPredictionError        :: V3 Float
               , _csFrame                  :: FrameT
               , _csSurpressCount          :: Int
               , _csFrames                 :: V.Vector FrameT
               , _csViewAngles             :: V3 Float
               , _csTime                   :: Int
               , _csLerpFrac               :: Float
               , _csRefDef                 :: RefDefT
               , _csVForward               :: V3 Float
               , _csVRight                 :: V3 Float
               , _csVUp                    :: V3 Float
               , _csLayout                 :: B.ByteString
               , _csInventory              :: UV.Vector Int
               , _csCinematicFile          :: B.ByteString
               , _csCinematicTime          :: Int
               , _csCinematicFrame         :: Int
               , _csCinematicPalette       :: B.ByteString
               , _csCinematicPaletteActive :: Bool
               , _csAttractLoop            :: Bool
               , _csServerCount            :: Int
               , _csGameDir                :: B.ByteString
               , _csPlayerNum              :: Int
               , _csConfigStrings          :: V.Vector B.ByteString
               , _csModelDraw              :: V.Vector ModelT
               , _csModelClip              :: V.Vector CModelT
               , _csSoundPrecache          :: V.Vector SfxT
               , _csImagePrecache          :: V.Vector ImageT
               , _csClientInfo             :: V.Vector ClientInfoT
               , _csBaseClientInfo         :: ClientInfoT
               }

data GLPolyT =
  GLPolyT { _glpNext           :: GLPolyT
          , _glpChain          :: GLPolyT
          , _glpNumVerts       :: Int
          , _glpFlags          :: Int
          , _glpPos            :: Int
          , _glpGetX           :: Int -> Quake Float
          , _glpSetX           :: Int -> Float -> Quake ()
          , _glpGetY           :: Int -> Quake Float
          , _glpSetY           :: Int -> Float -> Quake ()
          , _glpGetZ           :: Int -> Quake Float
          , _glpSetZ           :: Int -> Float -> Quake ()
          , _glpGetS1          :: Int -> Quake Float
          , _glpSetS1          :: Int -> Float -> Quake ()
          , _glpGetT1          :: Int -> Quake Float
          , _glpSetT1          :: Int -> Float -> Quake ()
          , _glpGetS2          :: Int -> Quake Float
          , _glpSetS2          :: Int -> Float -> Quake ()
          , _glpGetT2          :: Int -> Quake Float
          , _glpSetT2          :: Int -> Float -> Quake ()
          , _glpBeginScrolling :: Float -> Quake ()
          , _glpEndScrolling   :: Quake ()
          }

data MTexInfoT =
  MTexInfoT { _mtiVecs      :: (V4 Float, V4 Float)
            , _mtiFlags     :: Int
            , _mtiNumFrames :: Int
            , _mtiNext      :: MTexInfoT
            , _mtiImage     :: ImageT
            }

data ImageT =
  ImageT { _iId                   :: Int
         , _iName                 :: B.ByteString
         , _iType                 :: Int
         , _iWidth                :: Int
         , _iHeight               :: Int
         , _iUploadWidth          :: Int
         , _iUploadHeight         :: Int
         , _iRegistrationSequence :: Int
         , _iTextureChain         :: MSurfaceT
         , _iTexNum               :: Int
         , _iSL                   :: Float
         , _iTL                   :: Float
         , _iSH                   :: Float
         , _iTH                   :: Float
         , _iScrap                :: Bool
         , _iHasAlpha             :: Bool
         , _iPaletted             :: Bool
         }

data RefDefT =
  RefDefT { _rdX            :: Int
          , _rdY            :: Int
          , _rdWidth        :: Int
          , _rdHeight       :: Int
          , _rdFovX         :: Float
          , _rdFovY         :: Float
          , _rdViewOrg      :: V3 Float
          , _rdViewAngles   :: V3 Float
          , _rdBlend        :: V4 Float
          , _rdTime         :: Float
          , _rdRdFlags      :: Int
          , _rdAreaBits     :: UV.Vector Word8
          , _rdLightStyles  :: V.Vector LightStyleT
          , _rdNumEntities  :: Int
          , _rdEntities     :: V.Vector EntityT
          , _rdNumDLights   :: Int
          , _rdDLights      :: V.Vector DLightT
          , _rdNumParticles :: Int
          }

data EntityT =
  EntityT { _eModel      :: ModelT
          , _eAngles     :: V3 Float
          , _eOrigin     :: V3 Float
          , _eFrame      :: Int
          , _eOldOrigin  :: V3 Float
          , _eOldFrame   :: Int
          , _eBackLerp   :: Float
          , _eSkinNum    :: Int
          , _eLightStyle :: Int
          , _eAlpha      :: Float
          , _eSkin       :: ImageT
          , _enFlags     :: Int -- name clash with EdictT._eFlags
          }

data ModelT =
  ModelT { _mName                 :: B.ByteString
         , _mRegistrationSequence :: Int
         , _mType                 :: Int
         , _mNumFrames            :: Int
         , _mFlags                :: Int
         , _mMins                 :: V3 Float
         , _mMaxs                 :: V3 Float
         , _mRadius               :: Float
         , _mClipBox              :: Bool
         , _mClipMins             :: V3 Float
         , _mClipMaxs             :: V3 Float
         , _mFirstModelSurface    :: Int
         , _mNumModelSurfaces     :: Int
         , _mLightmap             :: Int
         , _mNumSubModels         :: Int
         , _mSubModels            :: Seq MModelT
         , _mNumPlanes            :: Int
         , _mPlanes               :: Seq CPlaneT
         , _mNumLeafs             :: Int
         , _mLeafs                :: Seq MLeafT
         , _mNumVertexes          :: Int
         , _mVertexes             :: Seq MVertexT
         , _mNumEdges             :: Int
         , _mEdges                :: Seq MEdgeT
         , _mNumNodes             :: Int
         , _mFirstNode            :: Int
         , _mNodes                :: Seq MNodeT
         , _mNumTexInfo           :: Int
         , _mTexInfo              :: Seq MTexInfoT
         , _mNumSurfaces          :: Int
         , _mSurfaces             :: Seq MSurfaceT
         , _mNumSurfEdges         :: Int
         , _mSurfEdges            :: Seq Int
         , _mNumMarkSurfaces      :: Int
         , _mMarkSurfaces         :: Seq MSurfaceT
         -- TODO: qfiles.dvis_t vis
         , _mLightdata            :: B.ByteString
         , _mSkins                :: Seq ImageT
         , _mExtraDataSize        :: Int
         , _mExtraData            :: Maybe B.ByteString
         }

data MSurfaceT =
  MSurfaceT { _msVisFrame           :: Int
            , _msPlane              :: CPlaneT
            , _msFlags              :: Int
            , _msFirstEdge          :: Int
            , _msNumEdges           :: Int
            , _msTextureMins        :: (Int16, Int16)
            , _msExtents            :: (Int16, Int16)
            , _msLightS             :: Int
            , _msLightT             :: Int
            , _msDLightS            :: Int
            , _msDLightT            :: Int
            , _msPolys              :: GLPolyT
            , _msTextureChain       :: MSurfaceT
            , _msLightmapChain      :: MSurfaceT
            , _msTexInfo            :: MTexInfoT
            , _msDLightFrame        :: Int
            , _msDLightBits         :: Int
            , _msLightmapTextureNum :: Int
            , _msStyles             :: B.ByteString
            , _msCachedLight        :: UV.Vector Float
            , _msSamples            :: B.ByteString
            }

data MLeafT =
  MLeafT { _mlCluster         :: Int
         , _mlArea            :: Int
         , _mlNumMarkSurfaces :: Int
         , _mlMarkIndex       :: Int
         , _mlMarkSurfaces    :: Seq MSurfaceT
         }

data ClientInfoT =
  ClientInfoT { _ciName        :: B.ByteString
              , _ciCInfo       :: B.ByteString
              , _ciSkin        :: ImageT
              , _ciIcon        :: ImageT
              , _ciIconName    :: B.ByteString
              , _ciModel       :: ModelT
              , _ciWeaponModel :: V.Vector ModelT
              }

data SCRGlobals =
  SCRGlobals { _scrConCurrent      :: Float
             , _scrConLines        :: Float
             , _scrInitialized     :: Bool
             , _scrDrawLoading     :: Int
             , _scrDirty           :: DirtyT
             , _scrOldDirty        :: (DirtyT, DirtyT)
             , _scrCrosshairPic    :: B.ByteString
             , _scrCrosshairWidth  :: Int
             , _scrCrosshairHeight :: Int
             }

data RefExportT =
  RefExportT { _reInit                :: Int -> Int -> Quake Bool
             , _reShutDown            :: Quake ()
             , _reBeginRegistration   :: B.ByteString -> Quake ()
             , _reRegisterModel       :: B.ByteString -> Quake (Maybe ModelT)
             , _reRegisterSkin        :: B.ByteString -> Quake (Maybe ImageT)
             , _reRegisterPic         :: B.ByteString -> Quake (Maybe ImageT)
             , _reSetSky              :: B.ByteString -> Float -> V3 Float -> Quake ()
             , _reEndRegistration     :: Quake ()
             , _reRenderFrame         :: RefDefT -> Quake ()
             , _reDrawGetPicSize      :: Int -> Int -> B.ByteString -> Quake ()
             , _reDrawPic             :: Int -> Int -> B.ByteString -> Quake ()
             , _reDrawStretchPic      :: Int -> Int -> Int -> Int -> B.ByteString -> Quake ()
             , _reDrawChar            :: Int -> Int -> Char -> Quake ()
             , _reDrawTileClear       :: Int -> Int -> Int -> Int -> B.ByteString -> Quake ()
             , _reDrawFill            :: Int -> Int -> Int -> Int -> Int -> Quake ()
             , _reDrawFadeScreen      :: Quake ()
             , _reDrawStretchRaw      :: Int -> Int -> Int -> Int -> Int -> Int -> B.ByteString -> Quake ()
             , _reCinematicSetPalette :: B.ByteString -> Quake ()
             , _reBeginFrame          :: Float -> Quake ()
             , _reEndFrame            :: Quake ()
             , _reAppActivate         :: Bool -> Quake ()
             , _reUpdateScreen        :: XCommandT -> Quake ()
             , _reApiVersion          :: Int
             , _reGetModeList         :: UV.Vector Int -- TODO: ???
             , _reGetKeyboardHandler  :: Int -- TODO: ???
             }

data NETGlobals =
  NETGlobals { _ngLoopbacks   :: (LoopbackT, LoopbackT)
             , _ngIpSockets   :: (Maybe Socket, Maybe Socket)
             , _ngNetLocalAdr :: NetAdrT
             }

data GItemT =
  GItemT { _giClassName       :: B.ByteString
         , _giPickup          :: Maybe EntInteract
         , _giUse             :: Maybe ItemUse
         , _giDrop            :: Maybe ItemDrop
         , _giWeaponThink     :: Maybe EntThink
         , _giPickupSound     :: Maybe B.ByteString
         , _giWorldModel      :: Maybe B.ByteString
         , _giWorldModelFlags :: Int
         , _giViewModel       :: Maybe B.ByteString
         , _giIcon            :: Maybe B.ByteString
         , _giPickupName      :: Maybe B.ByteString
         , _giCountWidth      :: Int
         , _giQuantity        :: Int
         , _giAmmo            :: Maybe B.ByteString
         , _giFlags           :: Int
         , _giWeaponModel     :: Int
         , _giInfo            :: Maybe GItemArmorT
         , _giTag             :: Int
         , _giPrecaches       :: B.ByteString
         , _giIndex           :: Int
         }

class SuperAdapter a where
    getID :: a -> B.ByteString

class EntInteractAdapter a where
    interact :: a -> (Lens' QuakeState EdictT -> Lens' QuakeState EdictT -> Quake Bool)

class ItemUseAdapter a where
    use :: a -> (Lens' QuakeState EdictT -> GItemT -> Quake ())

class ItemDropAdapter a where
    drop :: a -> (Lens' QuakeState EdictT -> GItemT -> Quake ())

class EntThinkAdapter a where
    think :: a -> (Lens' QuakeState EdictT -> Quake Bool)

data EntInteract = GenericEntInteract { _geiId :: B.ByteString, _geiInteract :: Lens' QuakeState EdictT -> Lens' QuakeState EdictT -> Quake Bool }

data ItemUse = GenericItemUse { _giuId :: B.ByteString, _giuUse :: Lens' QuakeState EdictT -> GItemT -> Quake () }

data ItemDrop = GenericItemDrop { _gidId :: B.ByteString, _gidDrop :: Lens' QuakeState EdictT -> GItemT -> Quake () }

data EntThink = GenericEntThink { _getId :: B.ByteString, _getThink :: Lens' QuakeState EdictT -> Quake Bool }

instance SuperAdapter EntInteract where
    getID (GenericEntInteract _id _) = _id

instance SuperAdapter ItemUse where
    getID (GenericItemUse _id _) = _id

instance SuperAdapter ItemDrop where
    getID (GenericItemDrop _id _) = _id

instance SuperAdapter EntThink where
    getID (GenericEntThink _id _) = _id

instance EntInteractAdapter EntInteract where
    interact (GenericEntInteract _ _interact) = _interact

instance ItemUseAdapter ItemUse where
    use (GenericItemUse _ _use) = _use

instance ItemDropAdapter ItemDrop where
    drop (GenericItemDrop _ _drop) = _drop

instance EntThinkAdapter EntThink where
    think (GenericEntThink _ _think) = _think

data ClientRespawnT =
  ClientRespawnT { _crCoopRespawn :: ClientPersistantT
                 , _crEnterFrame  :: Int
                 , _crScore       :: Int
                 , _crCmdAngles   :: V3 Float
                 , _crSpectator   :: Bool
                 }

data ClientPersistantT = 
  ClientPersistantT { _cpUserInfo        :: B.ByteString
                    , _cpNetName         :: B.ByteString
                    , _cpHand            :: Int
                    , _cpConnected       :: Bool
                    , _cpHealth          :: Int
                    , _cpMaxHealth       :: Int
                    , _cpSavedFlags      :: Int
                    , _cpSelectedItem    :: Int
                    , _cpInventory       :: UV.Vector Int
                    , _cpMaxBullets      :: Int
                    , _cpMaxShells       :: Int
                    , _cpMaxRockets      :: Int
                    , _cpMaxGrenades     :: Int
                    , _cpMaxCells        :: Int
                    , _cpMaxSlugs        :: Int
                    , _cpWeapon          :: GItemT
                    , _cpLastWeapon      :: GItemT
                    , _cpPowerCubes      :: Int
                    , _cpScore           :: Int
                    , _cpGameHelpChanged :: Int
                    , _cpHelpChanged     :: Int
                    , _cpSpectator       :: Bool
                    }
