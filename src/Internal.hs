{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ImpredicativeTypes #-}
module Internal where

import Control.Applicative
import Control.Concurrent.STM.TChan (TChan)
import Control.Lens (Zoom, zoom, Lens')
import Control.Lens.Internal.Zoom (Zoomed, Focusing)
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Int (Int16, Int32, Int64)
import Data.Sequence (Seq)
import Data.Word (Word8, Word16)
import Linear (V3, V4)
import System.IO (Handle)
import System.Random (StdGen)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Storable.Mutable as MSV
import qualified Data.Vector.Unboxed as UV
import qualified Graphics.UI.GLFW as GLFW

import Client.ClientStaticT
import Client.ConsoleT
import Client.DirtyT
import Client.DLightT
import Client.FrameT
import Client.KButtonT
import Client.LightStyleT
import Client.VidDefT
import Client.VidModeT
import Game.CmdAliasT
import Game.CModelT
import Game.CPlaneT
import Game.CSurfaceT
import Game.CVarT
import Game.GItemArmorT
import Game.MapSurfaceT
import Game.PMoveStateT
import Game.PlayerStateT
import Game.SpawnTempT
import Game.UserCmdT
import QCommon.CAreaT
import QCommon.CBrushSideT
import QCommon.CBrushT
import QCommon.CLeafT
import QCommon.CNodeT
import QCommon.FileLinkT
import QCommon.NetAdrT
import QCommon.NetChanT
import QCommon.PmlT
import QCommon.QFiles.BSP.DAreaPortalT
import QCommon.QFiles.BSP.DVisT
import QCommon.QFiles.MD2.DMdlT
import QCommon.QFiles.SP2.DSpriteT
import QCommon.SearchPathT
import QCommon.SizeBufT
import Render.GLConfigT
import Render.GLStateT
import Render.MEdgeT
import Render.MModelT
import Render.MVertexT
import Server.ChallengeT
import Server.ClientFrameT
import Sound.SfxT
import Sys.LoopbackT
import Sys.Socket

newtype QuakeS s a = Quake { unQuake :: StateT s (ExceptT B.ByteString IO) a }
                       deriving ( Functor
                                , Applicative
                                , Monad
                                , MonadIO
                                , MonadError B.ByteString
                                , MonadState s)

type Quake = QuakeS QuakeState

type instance Zoomed (QuakeS s) = Focusing (ExceptT B.ByteString IO)

instance Zoom (QuakeS s) (QuakeS t) s t where
    zoom l (Quake m) = Quake (zoom l m)

type XCommandT = Quake ()

type KeyFuncT = Int -> Quake B.ByteString

-- reference to gameBaseGlobals.gbGEdicts
newtype EdictReference = EdictReference Int deriving (Eq, Show)

-- reference to svGlobals.svServerStatic.ssClients
newtype ClientReference = ClientReference Int

-- reference to gameBaseGlobals.gbGame.glClients
newtype GClientReference = GClientReference Int

-- reference to cmGlobals.cmMapCModels
newtype CModelReference = CModelReference Int

-- reference to cmGlobals.cmMapPlanes
newtype CPlaneReference = CPlaneReference Int

-- reference to svGlobals.svLinks
newtype LinkReference = LinkReference Int

-- reference to gameBaseGlobals.gbItemList
newtype GItemReference = GItemReference Int deriving (Eq)

-- reference to menuGlobals.mgMenuFrameworks
newtype MenuFrameworkSReference = MenuFrameworkSReference Int

-- reference to menuGlobals.mgMenuItems
newtype MenuItemReference = MenuItemReference Int

-- reference to (fast/basic)RenderAPIGlobals.frGLTextures
newtype ImageReference = ImageReference Int

-- reference to globals.cl.cmds
newtype UserCmdReference = UserCmdReference Int

-- reference to clientGlobals.cgParticles
newtype CParticleReference = CParticleReference Int

newtype MSurfaceReference = MSurfaceReference Int

newtype MTexInfoReference = MTexInfoReference Int

newtype GLPolyReference = GLPolyReference Int

newtype MNodeReference = MNodeReference Int

newtype SfxReference = SfxReference Int

-- reference to (fast/basic)RenderAPIGlobals.(frModInline/frModKnown)
data ModelReference = ModInlineReference !Int | ModKnownReference !Int

data MNodeChild = MNodeChildReference !Int | MLeafChildReference !Int

data ModelExtra = AliasModelExtra DMdlT | SpriteModelExtra DSpriteT

data QuakeState =
  QuakeState { _globals               :: Globals
             , _comGlobals            :: ComGlobals
             , _cmdGlobals            :: CmdGlobals
             , _keyGlobals            :: KeyGlobals
             , _fsGlobals             :: FSGlobals
             , _svGlobals             :: SVGlobals
             , _gameBaseGlobals       :: GameBaseGlobals
             , _pMoveGlobals          :: PMoveGlobals
             , _scrGlobals            :: SCRGlobals
             , _netGlobals            :: NETGlobals
             , _cmGlobals             :: CMGlobals
             , _gameItemsGlobals      :: GameItemsGlobals
             , _mBerserkGlobals       :: MBerserkGlobals
             , _mBoss2Globals         :: MBoss2Globals
             , _mBoss31Globals        :: MBoss31Globals
             , _mBoss32Globals        :: MBoss32Globals
             , _mBrainGlobals         :: MBrainGlobals
             , _mChickGlobals         :: MChickGlobals
             , _mFlipperGlobals       :: MFlipperGlobals
             , _mFloatGlobals         :: MFloatGlobals
             , _mFlyerGlobals         :: MFlyerGlobals
             , _mGladiatorGlobals     :: MGladiatorGlobals
             , _mGunnerGlobals        :: MGunnerGlobals
             , _mHoverGlobals         :: MHoverGlobals
             , _mInfantryGlobals      :: MInfantryGlobals
             , _mInsaneGlobals        :: MInsaneGlobals
             , _mMedicGlobals         :: MMedicGlobals
             , _mMutantGlobals        :: MMutantGlobals
             , _mSoldierGlobals       :: MSoldierGlobals
             , _playerTrailGlobals    :: PlayerTrailGlobals
             , _vidGlobals            :: VIDGlobals
             , _inGlobals             :: INGlobals
             , _glfwbGlobals          :: GLFWbGlobals
             , _kbdGlobals            :: KBDGlobals
             , _basicRenderAPIGlobals :: BasicRenderAPIGlobals
             , _fastRenderAPIGlobals  :: FastRenderAPIGlobals
             , _particleTGlobals      :: ParticleTGlobals
             , _menuGlobals           :: MenuGlobals
             , _clientGlobals         :: ClientGlobals
             , _vGlobals              :: VGlobals
             , _netChannelGlobals     :: NetChannelGlobals
             , _clTEntGlobals         :: CLTEntGlobals
             }

data Globals =
  Globals { _curtime            :: !Int
          , _cmdWait            :: !Bool

          , _aliasCount         :: !Int
          , _cTraces            :: !Int
          , _cBrushTraces       :: !Int
          , _cPointContents     :: !Int
          , _serverState        :: !Int

          , _netMessage         :: SizeBufT
          , _netMessageBuffer   :: B.ByteString
          , _cmdText            :: SizeBufT
          , _deferTextBuf       :: B.ByteString -- length 8192
          , _cmdTextBuf         :: B.ByteString -- length 8192
          , _cmdAlias           :: Seq CmdAliasT

          , _timeBeforeGame     :: !Int
          , _timeAfterGame      :: !Int
          , _timeBeforeRef      :: !Int
          , _timeAfterRef       :: !Int

          , _logStatsFile       :: Maybe Handle

          , _cls                :: ClientStaticT
          , _cl                 :: ClientStateT
          , _clEntities         :: V.Vector CEntityT

          , _userInfoModified   :: !Bool

          , _cvarVars           :: M.Map B.ByteString CVarT
          , _con                :: ConsoleT
          , _vidDef             :: VidDefT
          , _re                 :: Maybe Renderer

          , _keyBindings        :: V.Vector (Maybe B.ByteString)
          , _keyDown            :: UV.Vector Bool
          , _chatTeam           :: !Bool
          , _chatBuffer         :: B.ByteString
          , _keyLines           :: V.Vector B.ByteString
          , _keyLinePos         :: !Int
          , _editLine           :: !Int

          , _scrVRect           :: VRectT
          , _sysFrameTime       :: !Int
          , _netFrom            :: NetAdrT

          , _vec3Origin         :: V3 Float

          , _rnd                :: StdGen
          }

data ComGlobals =
  ComGlobals { _cgComArgc       :: !Int
             , _cgComArgv       :: V.Vector B.ByteString
             , _cgRecursive     :: !Bool
             , _cgMsg           :: B.ByteString
             , _cgDebugContext  :: B.ByteString
             , _cgDebugContext2 :: B.ByteString
             , _cgRdTarget      :: !Int
             }

data CmdGlobals =
  CmdGlobals { _cgCmdFunctions :: Seq CmdFunctionT
             , _cgCmdArgc      :: !Int
             , _cgCmdArgv      :: V.Vector B.ByteString
             , _cgCmdArgs      :: B.ByteString
             }

data KeyGlobals =
  KeyGlobals { _kgAnyKeyDown  :: !Int
             , _kgKeyWaiting  :: !Int
             , _kgHistoryLine :: !Int
             , _kgShiftDown   :: !Bool
             , _kgKeyRepeats  :: UV.Vector Int
             , _kgMenuBound   :: UV.Vector Bool
             , _kgConsoleKeys :: UV.Vector Bool
             , _kgKeyNames    :: V.Vector (Maybe B.ByteString)
             }

data FSGlobals =
  FSGlobals { _fsGameDir         :: B.ByteString
            , _fsUserDir         :: B.ByteString
            , _fsLinks           :: Seq FileLinkT
            , _fsSearchPaths     :: [SearchPathT]
            , _fsBaseSearchPaths :: [SearchPathT]
            , _fsFileFromPak     :: !Int
            }

data SVGlobals =
  SVGlobals { _svMasterAdr            :: V.Vector NetAdrT
            , _svClient               :: Maybe ClientReference
            , _svServer               :: ServerT
            , _svServerStatic         :: ServerStaticT
            , _svPlayer               :: Maybe EdictReference
            , _svFirstMap             :: B.ByteString
            , _svMsgBuf               :: B.ByteString
            , _svNumAreaNodes         :: !Int
            , _svAreaNodes            :: V.Vector AreaNodeT
            , _svAreaMins             :: V3 Float
            , _svAreaMaxs             :: V3 Float
            , _svAreaList             :: V.Vector EdictReference
            , _svAreaCount            :: !Int
            , _svAreaMaxCount         :: !Int
            , _svAreaType             :: !Int
            , _svLeafs                :: UV.Vector Int
            , _svClusters             :: UV.Vector Int
            , _svTouch                :: V.Vector EdictReference
            , _svTouchList            :: V.Vector EdictReference
            , _svLinks                :: V.Vector LinkT
            , _svMsg                  :: SizeBufT
            , _svLeafsTmp             :: UV.Vector Int
            }

data CmdFunctionT =
  CmdFunctionT { _cfName     :: B.ByteString
               , _cfFunction :: Maybe XCommandT
               }

data EdictActionT =
  EdictActionT { _eaNextThink :: !Float
               , _eaPrethink  :: Maybe EntThink
               , _eaThink     :: Maybe EntThink
               , _eaBlocked   :: Maybe EntBlocked
               , _eaTouch     :: Maybe EntTouch
               , _eaUse       :: Maybe EntUse
               , _eaPain      :: Maybe EntPain
               , _eaDie       :: Maybe EntDie
               }

data EdictOtherT =
  EdictOtherT { _eoChain        :: Maybe EdictReference
              , _eoEnemy        :: Maybe EdictReference
              , _eoOldEnemy     :: Maybe EdictReference
              , _eoActivator    :: Maybe EdictReference
              , _eoGroundEntity :: Maybe EdictReference
              , _eoTeamChain    :: Maybe EdictReference
              , _eoTeamMaster   :: Maybe EdictReference
              , _eoMyNoise      :: Maybe EdictReference
              , _eoMyNoise2     :: Maybe EdictReference
              }

data EdictTimingT =
  EdictTimingT { _etTouchDebounceTime    :: !Float
               , _etPainDebounceTime     :: !Float
               , _etDamageDebounceTime   :: !Float
               , _etFlySoundDebounceTime :: !Float
               , _etLastMoveTime         :: !Float
               }

data EdictMinMaxT =
  EdictMinMaxT { _eMins   :: V3 Float
               , _eMaxs   :: V3 Float
               , _eAbsMin :: V3 Float
               , _eAbsMax :: V3 Float
               , _eSize   :: V3 Float
               }

data EdictInfoT =
  EdictInfoT { _eiModel        :: Maybe B.ByteString
             , _eiMessage      :: Maybe B.ByteString
             , _eiTarget       :: Maybe B.ByteString
             , _eiTargetName   :: Maybe B.ByteString
             , _eiKillTarget   :: Maybe B.ByteString
             , _eiTeam         :: Maybe B.ByteString
             , _eiPathTarget   :: Maybe B.ByteString
             , _eiDeathTarget  :: Maybe B.ByteString
             , _eiCombatTarget :: Maybe B.ByteString
             , _eiMap          :: Maybe B.ByteString
             }

data EdictPhysicsT =
  EdictPhysicsT { _eAngle       :: !Float
                , _eSpeed       :: !Float
                , _eAccel       :: !Float
                , _eDecel       :: !Float
                , _eMoveDir     :: V3 Float
                , _ePos1        :: V3 Float
                , _ePos2        :: V3 Float
                , _eVelocity    :: V3 Float
                , _eAVelocity   :: V3 Float
                , _eMass        :: !Int
                , _eAirFinished :: !Float
                , _eGravity     :: !Float
                , _eYawSpeed    :: !Float
                , _eIdealYaw    :: !Float
                }

data EdictStatusT =
  EdictStatusT { _eHealth         :: !Int
               , _eMaxHealth      :: !Int
               , _eGibHealth      :: !Int
               , _eDeadFlag       :: !Int
               , _eShowHostile    :: !Int
               , _ePowerArmorTime :: !Float
               , _eViewHeight     :: !Int
               , _eTakeDamage     :: !Int
               , _eDmg            :: !Int
               , _eRadiusDmg      :: !Int
               , _eDmgRadius      :: !Float
               }

-- had to split EdictT into smaller EdictXXX types in order
-- for makeLenses not to generate A LOT of code which eats up
-- A LOT of memory
data EdictT =
  EdictT { _eEntityState           :: EntityStateT
         , _eInUse                 :: !Bool
         , _eClassName             :: B.ByteString
         , _eLinkCount             :: !Int
         , _eArea                  :: LinkReference
         , _eNumClusters           :: !Int
         , _eClusterNums           :: UV.Vector Int
         , _eHeadNode              :: !Int
         , _eAreaNum               :: !Int
         , _eAreaNum2              :: !Int
         , _eSvFlags               :: !Int
         , _eSolid                 :: !Int
         , _eClipMask              :: !Int
         , _eMoveType              :: !Int
         , _eFlags                 :: !Int
         , _eFreeTime              :: !Float
         , _eSpawnFlags            :: !Int
         , _eTimeStamp             :: !Float
         , _eEdictPhysics          :: EdictPhysicsT
         , _eTargetEnt             :: Maybe EdictReference
         , _eGoalEntity            :: Maybe EdictReference
         , _eMoveTarget            :: Maybe EdictReference
         , _eEdictAction           :: EdictActionT
         , _eEdictTiming           :: EdictTimingT
         , _eEdictStatus           :: EdictStatusT
         , _eSounds                :: !Int
         , _eCount                 :: !Int
         , _eGroundEntityLinkCount :: !Int
         , _eEdictOther            :: EdictOtherT
         , _eNoiseIndex            :: !Int
         , _eNoiseIndex2           :: !Int
         , _eVolume                :: !Float
         , _eAttenuation           :: !Float
         , _eWait                  :: !Float
         , _eDelay                 :: !Float
         , _eRandom                :: !Float
         , _eTeleportTime          :: !Float
         , _eWaterType             :: !Int
         , _eWaterLevel            :: !Int
         , _eMoveOrigin            :: V3 Float
         , _eMoveAngles            :: V3 Float
         , _eLightLevel            :: !Int
         , _eStyle                 :: !Int
         , _eItem                  :: Maybe GItemReference
         , _eMoveInfo              :: MoveInfoT
         , _eMonsterInfo           :: MonsterInfoT
         , _eClient                :: Maybe GClientReference
         , _eOwner                 :: Maybe EdictReference
         , _eIndex                 :: !Int
         , _eEdictInfo             :: EdictInfoT
         , _eEdictMinMax           :: EdictMinMaxT
         }

data EntityStateT =
  EntityStateT { _esNumber         :: !Int
               , _esOrigin         :: V3 Float
               , _esAngles         :: V3 Float
               , _esOldOrigin      :: V3 Float
               , _esModelIndex     :: !Int
               , _esModelIndex2    :: !Int
               , _esModelIndex3    :: !Int
               , _esModelIndex4    :: !Int
               , _esFrame          :: !Int
               , _esSkinNum        :: !Int
               , _esEffects        :: !Int
               , _esRenderFx       :: !Int
               , _esSolid          :: !Int
               , _esSound          :: !Int
               , _esEvent          :: !Int
               , _esSurroundingEnt :: Maybe EdictReference
               }

data GClientT =
  GClientT { _gcPlayerState        :: PlayerStateT
           , _gcPing               :: !Int
           , _gcPers               :: ClientPersistantT
           , _gcResp               :: ClientRespawnT
           , _gcOldPMove           :: PMoveStateT
           , _gcShowScores         :: !Bool
           , _gcShowInventory      :: !Bool
           , _gcShowHelp           :: !Bool
           , _gcShowHelpIcon       :: !Bool
           , _gcAmmoIndex          :: !Int
           , _gcButtons            :: !Int
           , _gcOldButtons         :: !Int
           , _gcLatchedButtons     :: !Int
           , _gcWeaponThunk        :: !Bool
           , _gcNewWeapon          :: Maybe GItemReference
           , _gcDamageArmor        :: !Int
           , _gcDamagePArmor       :: !Int
           , _gcDamageBlood        :: !Int
           , _gcDamageKnockback    :: !Int
           , _gcDamageFrom         :: V3 Float
           , _gcKillerYaw          :: !Float
           , _gcWeaponState        :: !Int
           , _gcKickAngles         :: V3 Float
           , _gcKickOrigin         :: V3 Float
           , _gcVDmgRoll           :: !Float
           , _gcVDmgPitch          :: !Float
           , _gcVDmgTime           :: !Float
           , _gcFallTime           :: !Float
           , _gcFallValue          :: !Float
           , _gcDamageAlpha        :: !Float
           , _gcBonusAlpha         :: !Float
           , _gcDamageBlend        :: V3 Float
           , _gcVAngle             :: V3 Float
           , _gcBobTime            :: !Float
           , _gcOldViewAngles      :: V3 Float
           , _gcOldVelocity        :: V3 Float
           , _gcNextDrownTime      :: !Float
           , _gcOldWaterLevel      :: !Int
           , _gcBreatherSound      :: !Int
           , _gcMachinegunShots    :: !Int
           , _gcAnimEnd            :: !Int
           , _gcAnimPriority       :: !Int
           , _gcAnimDuck           :: !Bool
           , _gcAnimRun            :: !Bool
           , _gcQuadFrameNum       :: !Float
           , _gcInvincibleFrameNum :: !Float
           , _gcBreatherFrameNum   :: !Float
           , _gcEnviroFrameNum     :: !Float
           , _gcGrenadeBlewUp      :: !Bool
           , _gcGrenadeTime        :: !Float
           , _gcSilencerShots      :: !Int
           , _gcWeaponSound        :: !Int
           , _gcPickupMsgTime      :: !Float
           , _gcFloodLockTill      :: !Float
           , _gcFloodWhen          :: UV.Vector Float
           , _gcFloodWhenHead      :: !Int
           , _gcRespawnTime        :: !Float
           , _gcChaseTarget        :: Maybe EdictReference
           , _gcUpdateChase        :: !Bool
           , _gcIndex              :: !Int
           }

data ClientT =
  ClientT { _cState         :: !Int
          , _cUserInfo      :: B.ByteString
          , _cLastFrame     :: !Int
          , _cLastCmd       :: UserCmdT
          , _cCommandMsec   :: !Int
          , _cFrameLatency  :: UV.Vector Int
          , _cPing          :: !Int
          , _cMessageSize   :: UV.Vector Int
          , _cRate          :: !Int
          , _cSurpressCount :: !Int
          , _cEdict         :: Maybe EdictReference
          , _cName          :: B.ByteString
          , _cMessageLevel  :: !Int
          , _cDatagram      :: SizeBufT
          , _cDatagramBuf   :: B.ByteString
          , _cFrames        :: V.Vector ClientFrameT
          , _cDownload      :: Maybe B.ByteString
          , _cDownloadSize  :: !Int
          , _cDownloadCount :: !Int
          , _cLastMessage   :: !Int
          , _cLastConnect   :: !Int
          , _cChallenge     :: !Int
          , _cNetChan       :: NetChanT
          , _cServerIndex   :: !Int
          }

data ServerStaticT =
  ServerStaticT { _ssInitialized        :: !Bool
                , _ssRealTime           :: !Int
                , _ssMapCmd             :: B.ByteString
                , _ssSpawnCount         :: !Int
                , _ssClients            :: V.Vector ClientT
                , _ssNumClientEntities  :: !Int
                , _ssNextClientEntities :: !Int
                , _ssClientEntities     :: V.Vector EntityStateT
                , _ssLastHeartbeat      :: !Int
                , _ssChallenges         :: V.Vector ChallengeT
                , _ssDemoFile           :: Maybe Handle
                , _ssDemoMulticast      :: SizeBufT
                , _ssDemoMulticastBuf   :: B.ByteString
                }

data ServerT =
  ServerT { _sState         :: !Int
          , _sAttractLoop   :: !Bool
          , _sLoadGame      :: !Bool
          , _sTime          :: !Int
          , _sFrameNum      :: !Int
          , _sName          :: B.ByteString
          , _sModels        :: V.Vector CModelReference
          , _sConfigStrings :: V.Vector B.ByteString
          , _sBaselines     :: V.Vector EntityStateT
          , _sMulticast     :: SizeBufT
          , _sMulticastBuf  :: B.ByteString
          , _sDemoFile      :: Maybe Handle
          , _sTimeDemo      :: !Int
          }

data GameLocalsT =
  GameLocalsT { _glHelpMessage1 :: B.ByteString
              , _glHelpMessage2 :: B.ByteString
              , _glHelpChanged  :: !Int
              , _glClients      :: V.Vector GClientT
              , _glSpawnPoint   :: B.ByteString
              , _glMaxClients   :: !Int
              , _glMaxEntities  :: !Int
              , _glServerFlags  :: !Int
              , _glNumItems     :: !Int
              , _glAutosaved    :: !Bool
              }

data LevelLocalsT =
  LevelLocalsT { _llFrameNum             :: !Int
               , _llTime                 :: !Float
               , _llLevelName            :: B.ByteString
               , _llMapName              :: B.ByteString
               , _llNextMap              :: B.ByteString
               , _llIntermissionTime     :: !Float
               , _llChangeMap            :: B.ByteString
               , _llExitIntermission     :: !Bool
               , _llIntermissionOrigin   :: V3 Float
               , _llIntermissionAngle    :: V3 Float
               , _llSightClient          :: Maybe EdictReference
               , _llSightEntity          :: Maybe EdictReference
               , _llSightEntityFrameNum  :: !Int
               , _llSoundEntity          :: Maybe EdictReference
               , _llSoundEntityFrameNum  :: !Int
               , _llSound2Entity         :: Maybe EdictReference
               , _llSound2EntityFrameNum :: !Int
               , _llPicHealth            :: !Int
               , _llTotalSecrets         :: !Int
               , _llFoundSecrets         :: !Int
               , _llTotalGoals           :: !Int
               , _llFoundGoals           :: !Int
               , _llTotalMonsters        :: !Int
               , _llKilledMonsters       :: !Int
               , _llCurrentEntity        :: Maybe EdictReference
               , _llBodyQue              :: !Int
               , _llPowerCubes           :: !Int
               }

data GameImportT =
  GameImportT { _giBprintf            :: Int -> B.ByteString -> Quake ()
              , _giDprintf            :: B.ByteString -> Quake ()
              , _giCprintf            :: EdictReference -> Int -> B.ByteString -> Quake ()
              , _giCenterPrintf       :: EdictReference -> B.ByteString -> Quake ()
              , _giSound              :: Maybe EdictReference -> Int -> Int -> Float -> Float -> Float -> Quake ()
              , _giPositionedSound    :: Maybe (V3 Float) -> EdictReference -> Int -> Int -> Float -> Float -> Float -> Quake ()
              , _giConfigString       :: Int -> B.ByteString -> Quake ()
              , _giError              :: B.ByteString -> Quake ()
              , _giError2             :: Int -> B.ByteString -> Quake ()
              , _giModelIndex         :: Maybe B.ByteString -> Quake Int
              , _giSoundIndex         :: Maybe B.ByteString -> Quake Int
              , _giImageIndex         :: Maybe B.ByteString -> Quake Int
              , _giSetModel           :: EdictReference -> Maybe B.ByteString -> Quake ()
              , _giTrace              :: V3 Float -> Maybe (V3 Float) -> Maybe (V3 Float) -> V3 Float -> Maybe EdictReference -> Int -> Quake TraceT
              , _giPointContents      :: V3 Float -> Quake Int
              , _giInPHS              :: V3 Float -> V3 Float -> Quake Bool
              , _giSetAreaPortalState :: Int -> Bool -> Quake ()
              , _giAreasConnected     :: Int -> Int -> Quake Bool
              , _giLinkEntity         :: EdictReference -> Quake ()
              , _giUnlinkEntity       :: EdictReference -> Quake ()
              , _giBoxEdicts          :: V3 Float -> V3 Float -> Lens' QuakeState (V.Vector EdictReference) -> Int -> Int -> Quake Int
              , _giPMove              :: PMoveT -> Quake ()
              , _giMulticast          :: V3 Float -> Int -> Quake ()
              , _giUnicast            :: EdictReference -> Bool -> Quake ()
              , _giWriteByte          :: Int -> Quake ()
              , _giWriteShort         :: Int -> Quake ()
              , _giWriteString        :: B.ByteString -> Quake ()
              , _giWritePosition      :: V3 Float -> Quake ()
              , _giWriteDir           :: V3 Float -> Quake ()
              , _giCVar               :: B.ByteString -> B.ByteString -> Int -> Quake (Maybe CVarT)
              , _giCVarSet            :: B.ByteString -> B.ByteString -> Quake CVarT
              , _giCVarForceSet       :: B.ByteString -> B.ByteString -> Quake CVarT
              , _giArgc               :: Quake Int
              , _giArgv               :: Int -> Quake B.ByteString
              , _giArgs               :: Quake B.ByteString
              , _giAddCommandString   :: B.ByteString -> Quake ()
              }

data TraceT =
  TraceT { _tAllSolid   :: !Bool
         , _tStartSolid :: !Bool
         , _tFraction   :: !Float
         , _tEndPos     :: V3 Float
         , _tPlane      :: CPlaneT
         , _tSurface    :: Maybe CSurfaceT
         , _tContents   :: !Int
         , _tEnt        :: Maybe EdictReference
         }

data PMoveT =
  PMoveT { _pmState         :: PMoveStateT
         , _pmCmd           :: UserCmdT
         , _pmSnapInitial   :: !Bool
         , _pmNumTouch      :: !Int
         , _pmTouchEnts     :: V.Vector EdictReference
         , _pmViewAngles    :: V3 Float
         , _pmViewHeight    :: !Float
         , _pmMins          :: V3 Float
         , _pmMaxs          :: V3 Float
         , _pmGroundEntity  :: Maybe EdictReference
         , _pmWaterType     :: !Int
         , _pmWaterLevel    :: !Int
         , _pmTrace         :: V3 Float -> V3 Float -> V3 Float -> V3 Float -> Quake (Maybe TraceT)
         , _pmPointContents :: V3 Float -> Int
         }

data GameBaseGlobals =
  GameBaseGlobals { _gbDummyPlane        :: CPlaneT
                  , _gbGame              :: GameLocalsT
                  , _gbLevel             :: LevelLocalsT
                  , _gbGameImport        :: GameImportT
                  , _gbSpawnTemp         :: SpawnTempT
                  , _gbSmMeatIndex       :: !Int
                  , _gbSndFry            :: !Int
                  , _gbMeansOfDeath      :: !Int
                  , _gbNumEdicts         :: !Int
                  , _gbGEdicts           :: V.Vector EdictT
                  , _gbItemList          :: V.Vector GItemT
                  , _gbPushed            :: V.Vector PushedT
                  , _gbPushedP           :: !Int
                  , _gbObstacle          :: Maybe EdictReference
                  , _gbCYes              :: !Int
                  , _gbCNo               :: !Int
                  , _gbTouch             :: V.Vector EdictReference
                  , _gbIsQuad            :: Bool
                  , _gbIsSilenced        :: Int
                  , _gbCurrentPlayer     :: Maybe EdictReference
                  , _gbCurrentClient     :: Maybe GClientReference
                  , _gbForward           :: V3 Float
                  , _gbRight             :: V3 Float
                  , _gbUp                :: V3 Float
                  , _gbXYSpeed           :: Float
                  , _gbBobMove           :: Float
                  , _gbBobCycle          :: Int
                  , _gbBobFracSin        :: Float
                  , _gbXxxi              :: Int
                  }

data PMoveGlobals =
  PMoveGlobals { _pmPM              :: PMoveT
               , _pmPML             :: PmlT
               , _pmPlanes          :: V.Vector (V3 Float)
               , _pmStopSpeed       :: !Float
               , _pmMaxSpeed        :: !Float
               , _pmDuckSpeed       :: !Float
               , _pmAccelerate      :: !Float
               , _pmAirAccelerate   :: !Float
               , _pmWaterAccelerate :: !Float
               , _pmFriction        :: !Float
               , _pmWaterFriction   :: !Float
               , _pmWaterSpeed      :: !Float
               }

data MonsterInfoT =
  MonsterInfoT { _miCurrentMove     :: Maybe MMoveT
               , _miAIFlags         :: !Int
               , _miNextFrame       :: !Int
               , _miScale           :: !Float
               , _miStand           :: Maybe EntThink
               , _miIdle            :: Maybe EntThink
               , _miSearch          :: Maybe EntThink
               , _miWalk            :: Maybe EntThink
               , _miRun             :: Maybe EntThink
               , _miDodge           :: Maybe EntDodge
               , _miAttack          :: Maybe EntThink
               , _miMelee           :: Maybe EntThink
               , _miSight           :: Maybe EntInteract
               , _miCheckAttack     :: Maybe EntThink
               , _miPauseTime       :: !Float
               , _miAttackFinished  :: !Float
               , _miSavedGoal       :: V3 Float
               , _miSearchTime      :: !Float
               , _miTrailTime       :: !Float
               , _miLastSighting    :: V3 Float
               , _miAttackState     :: !Int
               , _miLefty           :: !Int
               , _miIdleTime        :: !Float
               , _miLinkCount       :: !Int
               , _miPowerArmorType  :: !Int
               , _miPowerArmorPower :: !Int
               }

data ClientStateT =
  ClientStateT { _csTimeOutCount           :: !Int
               , _csTimeDemoFrames         :: !Int
               , _csTimeDemoStart          :: !Int
               , _csRefreshPrepped         :: !Bool
               , _csSoundPrepped           :: !Bool
               , _csForceRefDef            :: !Bool
               , _csParseEntities          :: !Int
               , _csCmd                    :: UserCmdT
               , _csCmds                   :: V.Vector UserCmdT
               , _csCmdTime                :: UV.Vector Int
               , _csPredictedOrigins       :: UV.Vector (V3 Int16)
               , _csPredictedStep          :: !Float
               , _csPredictedStepTime      :: !Int
               , _csPredictedOrigin        :: V3 Float
               , _csPredictedAngles        :: V3 Float
               , _csPredictionError        :: V3 Float
               , _csFrame                  :: FrameT
               , _csSurpressCount          :: !Int
               , _csFrames                 :: V.Vector FrameT
               , _csViewAngles             :: V3 Float
               , _csTime                   :: !Int
               , _csLerpFrac               :: !Float
               , _csRefDef                 :: RefDefT
               , _csVForward               :: V3 Float
               , _csVRight                 :: V3 Float
               , _csVUp                    :: V3 Float
               , _csLayout                 :: B.ByteString
               , _csInventory              :: UV.Vector Int
               , _csCinematicFile          :: Maybe B.ByteString
               , _csCinematicTime          :: !Int
               , _csCinematicFrame         :: !Int
               , _csCinematicPalette       :: B.ByteString
               , _csCinematicPaletteActive :: !Bool
               , _csAttractLoop            :: !Bool
               , _csServerCount            :: !Int
               , _csGameDir                :: B.ByteString
               , _csPlayerNum              :: !Int
               , _csConfigStrings          :: V.Vector B.ByteString
               , _csModelDraw              :: V.Vector (Maybe ModelReference)
               , _csModelClip              :: V.Vector (Maybe CModelReference)
               , _csSoundPrecache          :: V.Vector SfxT
               , _csImagePrecache          :: V.Vector (Maybe ImageReference)
               , _csClientInfo             :: V.Vector ClientInfoT
               , _csBaseClientInfo         :: ClientInfoT
               }

data GLPolyT =
  GLPolyT { _glpNext           :: Maybe GLPolyReference
          , _glpChain          :: Maybe GLPolyReference
          , _glpNumVerts       :: !Int
          , _glpFlags          :: !Int
          , _glpPos            :: !Int
          }

data MTexInfoT =
  MTexInfoT { _mtiVecs      :: (V4 Float, V4 Float)
            , _mtiFlags     :: !Int
            , _mtiNumFrames :: !Int
            , _mtiNext      :: Maybe Int
            , _mtiImage     :: Maybe ImageReference
            }

data ImageT =
  ImageT { _iId                   :: !Int
         , _iName                 :: B.ByteString
         , _iType                 :: !Int
         , _iWidth                :: !Int
         , _iHeight               :: !Int
         , _iUploadWidth          :: !Int
         , _iUploadHeight         :: !Int
         , _iRegistrationSequence :: !Int
         , _iTextureChain         :: Maybe MSurfaceT
         , _iTexNum               :: !Int
         , _iSL                   :: !Float
         , _iTL                   :: !Float
         , _iSH                   :: !Float
         , _iTH                   :: !Float
         , _iScrap                :: !Bool
         , _iHasAlpha             :: !Bool
         , _iPaletted             :: !Bool
         }

data RefDefT =
  RefDefT { _rdX            :: !Int
          , _rdY            :: !Int
          , _rdWidth        :: !Int
          , _rdHeight       :: !Int
          , _rdFovX         :: !Float
          , _rdFovY         :: !Float
          , _rdViewOrg      :: V3 Float
          , _rdViewAngles   :: V3 Float
          , _rdBlend        :: V4 Float
          , _rdTime         :: !Float
          , _rdRdFlags      :: !Int
          , _rdAreaBits     :: UV.Vector Word8
          , _rdLightStyles  :: V.Vector LightStyleT
          , _rdNumEntities  :: !Int
          , _rdEntities     :: V.Vector EntityT
          , _rdNumDLights   :: !Int
          , _rdDLights      :: V.Vector DLightT
          , _rdNumParticles :: !Int
          }

data EntityT =
  EntityT { _eModel      :: Maybe Int -- index to some modelT vector
          , _eAngles     :: V3 Float
          , _eOrigin     :: V3 Float
          , _eFrame      :: !Int
          , _eOldOrigin  :: V3 Float
          , _eOldFrame   :: !Int
          , _eBackLerp   :: !Float
          , _eSkinNum    :: !Int
          , _eLightStyle :: !Int
          , _eAlpha      :: !Float
          , _eSkin       :: Maybe Int -- index to some imageT vector
          , _enFlags     :: !Int -- name clash with EdictT._eFlags
          }

data ModelT =
  ModelT { _mName                 :: B.ByteString
         , _mRegistrationSequence :: !Int
         , _mType                 :: !Int
         , _mNumFrames            :: !Int
         , _mFlags                :: !Int
         , _mMins                 :: V3 Float
         , _mMaxs                 :: V3 Float
         , _mRadius               :: !Float
         , _mClipBox              :: !Bool
         , _mClipMins             :: V3 Float
         , _mClipMaxs             :: V3 Float
         , _mFirstModelSurface    :: !Int
         , _mNumModelSurfaces     :: !Int
         , _mLightmap             :: !Int
         , _mNumSubModels         :: !Int
         , _mSubModels            :: V.Vector MModelT
         , _mNumPlanes            :: !Int
         , _mPlanes               :: V.Vector CPlaneT
         , _mNumLeafs             :: !Int
         , _mLeafs                :: V.Vector MLeafT
         , _mNumVertexes          :: !Int
         , _mVertexes             :: V.Vector MVertexT
         , _mNumEdges             :: !Int
         , _mEdges                :: V.Vector MEdgeT
         , _mNumNodes             :: !Int
         , _mFirstNode            :: !Int
         , _mNodes                :: V.Vector MNodeT
         , _mNumTexInfo           :: !Int
         , _mTexInfo              :: V.Vector MTexInfoT
         , _mNumSurfaces          :: !Int
         , _mSurfaces             :: V.Vector MSurfaceT
         , _mNumSurfEdges         :: !Int
         , _mSurfEdges            :: V.Vector Int
         , _mNumMarkSurfaces      :: !Int
         , _mMarkSurfaces         :: V.Vector MSurfaceT
         , _mVis                  :: Maybe DVisT
         , _mLightdata            :: Maybe B.ByteString
         , _mSkins                :: V.Vector (Maybe ImageReference)
         , _mExtraDataSize        :: !Int
         , _mExtraData            :: Maybe ModelExtra
         }

data MSurfaceT =
  MSurfaceT { _msVisFrame           :: !Int
            , _msPlane              :: Maybe CPlaneT
            , _msFlags              :: !Int
            , _msFirstEdge          :: !Int
            , _msNumEdges           :: !Int
            , _msTextureMins        :: (Int16, Int16)
            , _msExtents            :: (Int16, Int16)
            , _msLightS             :: !Int
            , _msLightT             :: !Int
            , _msDLightS            :: !Int
            , _msDLightT            :: !Int
            , _msPolys              :: Maybe GLPolyReference
            , _msTextureChain       :: Maybe MSurfaceReference
            , _msLightmapChain      :: Maybe MSurfaceReference
            , _msTexInfo            :: MTexInfoT -- !(Maybe (ModelReference, MTexInfoReference))
            , _msDLightFrame        :: !Int
            , _msDLightBits         :: !Int
            , _msLightmapTextureNum :: !Int
            , _msStyles             :: B.ByteString
            , _msCachedLight        :: UV.Vector Float
            , _msSamples            :: Maybe B.ByteString
            }

data MLeafT =
  MLeafT { _mlContents        :: !Int
         , _mlVisFrame        :: !Int
         , _mlMins            :: V3 Float
         , _mlMaxs            :: V3 Float
         , _mlParent          :: Maybe MNodeReference
         , _mlCluster         :: !Int
         , _mlArea            :: !Int
         , _mlNumMarkSurfaces :: !Int
         , _mlMarkIndex       :: !Int
         , _mlMarkSurfaces    :: V.Vector MSurfaceT
         }

data ClientInfoT =
  ClientInfoT { _ciName        :: B.ByteString
              , _ciCInfo       :: B.ByteString
              , _ciSkin        :: Maybe ImageReference -- index to some imageT vector
              , _ciIcon        :: Maybe ImageReference -- index to some imageT vector
              , _ciIconName    :: B.ByteString
              , _ciModel       :: Maybe ModelReference -- index to some modelT vector
              , _ciWeaponModel :: V.Vector (Maybe ModelReference) -- index to some modelT vector
              }

data SCRGlobals =
  SCRGlobals { _scrConCurrent      :: !Float
             , _scrConLines        :: !Float
             , _scrInitialized     :: !Bool
             , _scrDrawLoading     :: !Int
             , _scrDirty           :: DirtyT
             , _scrOldDirty        :: (DirtyT, DirtyT)
             , _scrCrosshairPic    :: B.ByteString
             , _scrCrosshairWidth  :: !Int
             , _scrCrosshairHeight :: !Int
             , _scrCenterTimeOff   :: !Float
             , _scrLastFrames      :: !Int
             , _scrLastTime        :: !Int
             , _scrFPSValue        :: B.ByteString
             , _scrCin             :: CinematicsT
             }

data RefExportT =
  RefExportT { _reInit                :: Int -> Int -> Quake Bool
             , _reShutDown            :: Quake ()
             , _reBeginRegistration   :: B.ByteString -> Quake ()
             , _reRegisterModel       :: B.ByteString -> Quake (Maybe ModelReference)
             , _reRegisterSkin        :: B.ByteString -> Quake (Maybe ImageReference)
             , _reRegisterPic         :: B.ByteString -> Quake (Maybe ImageReference)
             , _reSetSky              :: B.ByteString -> Float -> V3 Float -> Quake ()
             , _reEndRegistration     :: Quake ()
             , _reRenderFrame         :: RefDefT -> Quake ()
             , _reDrawGetPicSize      :: B.ByteString -> Quake (Maybe (Int, Int))
             , _reDrawPic             :: Int -> Int -> B.ByteString -> Quake ()
             , _reDrawStretchPic      :: Int -> Int -> Int -> Int -> B.ByteString -> Quake ()
             , _reDrawChar            :: Int -> Int -> Int -> Quake ()
             , _reDrawTileClear       :: Int -> Int -> Int -> Int -> B.ByteString -> Quake ()
             , _reDrawFill            :: Int -> Int -> Int -> Int -> Int -> Quake ()
             , _reDrawFadeScreen      :: Quake ()
             , _reDrawStretchRaw      :: Int -> Int -> Int -> Int -> Int -> Int -> B.ByteString -> Quake ()
             , _reCinematicSetPalette :: Maybe B.ByteString -> Quake ()
             , _reBeginFrame          :: Float -> Quake ()
             , _reEndFrame            :: Quake ()
             , _reAppActivate         :: Bool -> Quake ()
             , _reUpdateScreen        :: XCommandT -> Quake ()
             , _reApiVersion          :: Int
             , _reGetModeList         :: Quake (V.Vector VideoMode)
             , _reGetKeyboardHandler  :: KBD
             }

data NETGlobals =
  NETGlobals { _ngLoopbackClient :: LoopbackT
             , _ngLoopbackServer :: LoopbackT
             , _ngIpSocketClient :: Maybe Socket
             , _ngIpSocketServer :: Maybe Socket
             , _ngSend           :: SizeBufT
             }

data GItemT =
  GItemT { _giClassName       :: B.ByteString
         , _giPickup          :: Maybe EntInteract
         , _giUse             :: Maybe ItemUse
         , _giDrop            :: Maybe ItemDrop
         , _giWeaponThink     :: Maybe EntThink
         , _giPickupSound     :: Maybe B.ByteString
         , _giWorldModel      :: Maybe B.ByteString
         , _giWorldModelFlags :: !Int
         , _giViewModel       :: Maybe B.ByteString
         , _giIcon            :: Maybe B.ByteString
         , _giPickupName      :: Maybe B.ByteString
         , _giCountWidth      :: !Int
         , _giQuantity        :: !Int
         , _giAmmo            :: Maybe B.ByteString
         , _giFlags           :: !Int
         , _giWeaponModel     :: !Int
         , _giInfo            :: Maybe GItemArmorT
         , _giTag             :: !Int
         , _giPrecaches       :: B.ByteString
         , _giIndex           :: !Int
         }

class SuperAdapter a where
    getID :: a -> B.ByteString

class EntInteractAdapter a where
    entInteract :: a -> EdictReference -> EdictReference -> Quake Bool

class EntThinkAdapter a where
    think :: a -> EdictReference -> Quake Bool

class EntBlockedAdapter a where
    blocked :: a -> EdictReference -> EdictReference -> Quake ()

class EntDodgeAdapter a where
    dodge :: a -> EdictReference -> EdictReference -> Float -> Quake ()

class EntTouchAdapter a where
    touch :: a -> EdictReference -> EdictReference -> CPlaneT -> Maybe CSurfaceT -> Quake ()

class EntUseAdapter a where
    entUse :: a -> EdictReference -> Maybe EdictReference -> Maybe EdictReference -> Quake ()

class EntPainAdapter a where
    pain :: a -> EdictReference -> EdictReference -> Float -> Int -> Quake ()

class EntDieAdapter a where
    die :: a -> EdictReference -> EdictReference -> EdictReference -> Int -> V3 Float -> Quake ()

class ItemUseAdapter a where
    itemUse :: a -> EdictReference -> GItemT -> Quake ()

class ItemDropAdapter a where
    drop :: a -> EdictReference -> GItemReference -> Quake ()

class AIAdapter a where
    ai :: a -> EdictReference -> Float -> Quake ()

data EntInteract =
    PickupArmor B.ByteString (EdictReference -> EdictReference -> Quake Bool)
  | PickupPowerArmor B.ByteString (EdictReference -> EdictReference -> Quake Bool)
  | PickupHealth B.ByteString (EdictReference -> EdictReference -> Quake Bool)
  | PickupAdrenaline B.ByteString (EdictReference -> EdictReference -> Quake Bool)
  | PickupAncientHead B.ByteString (EdictReference -> EdictReference -> Quake Bool)
  | GenericEntInteract { _geiId :: B.ByteString
                       , _geiInteract :: EdictReference -> EdictReference -> Quake Bool
                       }

data EntThink =
  GenericEntThink { _gethId :: B.ByteString
                  , _gethThink :: EdictReference -> Quake Bool
                  }

data EntBlocked =
  GenericEntBlocked { _gebId :: B.ByteString
                    , _gebBlocked :: EdictReference -> EdictReference -> Quake ()
                    }

data EntDodge =
  GenericEntDodge { _gedoId :: B.ByteString
                  , _gedoDodge :: EdictReference -> EdictReference -> Float -> Quake ()
                  }

data EntTouch =
  GenericEntTouch { _getId :: B.ByteString
                  , _getTouch :: EdictReference -> EdictReference -> CPlaneT -> Maybe CSurfaceT -> Quake ()
                  }

data EntUse =
    FuncExplosiveUse B.ByteString (EdictReference -> Maybe EdictReference -> Maybe EdictReference -> Quake ())
  | GenericEntUse { _geuId :: B.ByteString
                , _geuUse :: EdictReference -> Maybe EdictReference -> Maybe EdictReference -> Quake ()
                }

data EntPain =
  GenericEntPain { _gepId :: B.ByteString
                 , _gepPain :: EdictReference -> EdictReference -> Float -> Int -> Quake ()
                 }

data EntDie =
  GenericEntDie { _gedId :: B.ByteString
                , _gedDie :: EdictReference -> EdictReference -> EdictReference -> Int -> V3 Float -> Quake ()
                }

data ItemUse =
  GenericItemUse { _giuId :: B.ByteString
                 , _giuUse :: EdictReference -> GItemT -> Quake ()
                 }

data ItemDrop =
  GenericItemDrop { _gidId :: B.ByteString
                  , _gidDrop :: EdictReference -> GItemReference -> Quake ()
                  }

data AI =
  GenericAI { _gaiId :: B.ByteString
            , _gaiAi :: EdictReference -> Float -> Quake ()
            }

instance SuperAdapter EntInteract where
    getID (PickupArmor _id _) = _id
    getID (PickupPowerArmor _id _) = _id
    getID (PickupHealth _id _) = _id
    getID (PickupAdrenaline _id _) = _id
    getID (PickupAncientHead _id _) = _id
    getID (GenericEntInteract _id _) = _id

instance SuperAdapter EntThink where
    getID (GenericEntThink _id _) = _id

instance SuperAdapter EntBlocked where
    getID (GenericEntBlocked _id _) = _id

instance SuperAdapter EntDodge where
    getID (GenericEntDodge _id _) = _id

instance SuperAdapter EntTouch where
    getID (GenericEntTouch _id _) = _id

instance SuperAdapter EntUse where
    getID (FuncExplosiveUse _id _) = _id
    getID (GenericEntUse _id _) = _id

instance SuperAdapter EntPain where
    getID (GenericEntPain _id _) = _id

instance SuperAdapter EntDie where
    getID (GenericEntDie _id _) = _id

instance SuperAdapter ItemUse where
    getID (GenericItemUse _id _) = _id

instance SuperAdapter ItemDrop where
    getID (GenericItemDrop _id _) = _id

instance SuperAdapter AI where
    getID (GenericAI _id _) = _id

instance EntInteractAdapter EntInteract where
    entInteract (PickupArmor _ _interact) = _interact
    entInteract (PickupPowerArmor _ _interact) = _interact
    entInteract (PickupHealth _ _interact) = _interact
    entInteract (PickupAdrenaline _ _interact) = _interact
    entInteract (PickupAncientHead _ _interact) = _interact
    entInteract (GenericEntInteract _ _interact) = _interact

instance EntThinkAdapter EntThink where
    think (GenericEntThink _ _think) = _think

instance EntBlockedAdapter EntBlocked where
    blocked (GenericEntBlocked _ _blocked) = _blocked

instance EntDodgeAdapter EntDodge where
    dodge (GenericEntDodge _ _dodge) = _dodge

instance EntTouchAdapter EntTouch where
    touch (GenericEntTouch _ _touch) = _touch

instance EntUseAdapter EntUse where
    entUse (FuncExplosiveUse _ _use) = _use
    entUse (GenericEntUse _ _use) = _use

instance EntPainAdapter EntPain where
    pain (GenericEntPain _ _pain) = _pain

instance EntDieAdapter EntDie where
    die (GenericEntDie _ _die) = _die

instance ItemUseAdapter ItemUse where
    itemUse (GenericItemUse _ _itemUse) = _itemUse

instance ItemDropAdapter ItemDrop where
    drop (GenericItemDrop _ _drop) = _drop

instance AIAdapter AI where
    ai (GenericAI _ _ai) = _ai

data ClientRespawnT =
  ClientRespawnT { _crCoopRespawn :: ClientPersistantT
                 , _crEnterFrame  :: !Int
                 , _crScore       :: !Int
                 , _crCmdAngles   :: V3 Float
                 , _crSpectator   :: !Bool
                 }

data ClientPersistantT =
  ClientPersistantT { _cpUserInfo        :: B.ByteString
                    , _cpNetName         :: B.ByteString
                    , _cpHand            :: !Int
                    , _cpConnected       :: !Bool
                    , _cpHealth          :: !Int
                    , _cpMaxHealth       :: !Int
                    , _cpSavedFlags      :: !Int
                    , _cpSelectedItem    :: !Int
                    , _cpInventory       :: UV.Vector Int
                    , _cpMaxBullets      :: !Int
                    , _cpMaxShells       :: !Int
                    , _cpMaxRockets      :: !Int
                    , _cpMaxGrenades     :: !Int
                    , _cpMaxCells        :: !Int
                    , _cpMaxSlugs        :: !Int
                    , _cpWeapon          :: Maybe GItemReference
                    , _cpLastWeapon      :: Maybe GItemReference
                    , _cpPowerCubes      :: !Int
                    , _cpScore           :: !Int
                    , _cpGameHelpChanged :: !Int
                    , _cpHelpChanged     :: !Int
                    , _cpSpectator       :: !Bool
                    }

data CMGlobals =
  CMGlobals { _cmCheckCount      :: !Int
            , _cmMapName         :: B.ByteString
            , _cmNumBrushSides   :: !Int
            , _cmMapBrushSides   :: V.Vector CBrushSideT
            , _cmNumTexInfo      :: !Int
            , _cmMapSurfaces     :: V.Vector MapSurfaceT
            , _cmNumPlanes       :: !Int
            , _cmMapPlanes       :: V.Vector CPlaneT
            , _cmNumNodes        :: !Int
            , _cmMapNodes        :: V.Vector CNodeT
            , _cmNumLeafs        :: !Int
            , _cmMapLeafs        :: V.Vector CLeafT
            , _cmEmptyLeaf       :: !Int
            , _cmSolidLeaf       :: !Int
            , _cmNumLeafBrushes  :: !Int
            , _cmMapLeafBrushes  :: UV.Vector Word16
            , _cmNumCModels      :: !Int
            , _cmMapCModels      :: V.Vector CModelT
            , _cmNumBrushes      :: !Int
            , _cmMapBrushes      :: V.Vector CBrushT
            , _cmNumVisibility   :: !Int
            , _cmMapVisibility   :: BL.ByteString
            , _cmMapVis          :: DVisT
            , _cmNumEntityChars  :: !Int
            , _cmMapEntityString :: B.ByteString
            , _cmNumAreas        :: !Int
            , _cmMapAreas        :: V.Vector CAreaT
            , _cmNumAreaPortals  :: !Int
            , _cmMapAreaPortals  :: V.Vector DAreaPortalT
            , _cmNumClusters     :: !Int
            , _cmFloodValid      :: !Int
            , _cmPortalOpen      :: UV.Vector Bool
            , _cmCModBase        :: Maybe BL.ByteString
            , _cmChecksum        :: !Int
            , _cmLastChecksum    :: !Int
            , _cmDebugLoadMap    :: !Bool
            , _cmBoxHeadNode     :: !Int
            , _cmLeafCount       :: !Int
            , _cmLeafMaxCount    :: !Int
            , _cmLeafMins        :: V3 Float
            , _cmLeafMaxs        :: V3 Float
            , _cmLeafTopNode     :: !Int
            , _cmTraceStart      :: V3 Float
            , _cmTraceEnd        :: V3 Float
            , _cmTraceMins       :: V3 Float
            , _cmTraceMaxs       :: V3 Float
            , _cmTraceExtents    :: V3 Float
            , _cmTraceTrace      :: TraceT
            , _cmTraceContents   :: !Int
            , _cmTraceIsPoint    :: !Bool
            , _cmLeafs           :: UV.Vector Int -- tmp for CM.boxTrace
            }

data MoveInfoT =
  MoveInfoT { _miStartOrigin       :: V3 Float
            , _miStartAngles       :: V3 Float
            , _miEndOrigin         :: V3 Float
            , _miEndAngles         :: V3 Float
            , _miSoundStart        :: !Int
            , _miSoundMiddle       :: !Int
            , _miSoundEnd          :: !Int
            , _miAccel             :: !Float
            , _miSpeed             :: !Float
            , _miDecel             :: !Float
            , _miDistance          :: !Float
            , _miWait              :: !Float
            , _miState             :: !Int
            , _miDir               :: V3 Float
            , _miCurrentSpeed      :: !Float
            , _miMoveSpeed         :: !Float
            , _miNextSpeed         :: !Float
            , _miRemainingDistance :: !Float
            , _miDecelDistance     :: !Float
            , _miEndFunc           :: Maybe EntThink
            }

data CEntityT =
  CEntityT { _ceBaseline    :: EntityStateT
           , _ceCurrent     :: EntityStateT
           , _cePrev        :: EntityStateT
           , _ceServerFrame :: !Int
           , _ceTrailCount  :: !Int
           , _ceLerpOrigin  :: V3 Float
           , _ceFlyStopTime :: !Int
           }

data AreaNodeT =
  AreaNodeT { _anAxis          :: !Int
            , _anDist          :: !Float
            , _anChildren      :: (Maybe Int, Maybe Int) -- indexes to svGlobals.svAreaNodes
            , _anTriggerEdicts :: LinkReference
            , _anSolidEdicts   :: LinkReference
            }

data LinkT =
  LinkT { _lIndex :: !Int
        , _lPrev  :: Maybe LinkReference
        , _lNext  :: Maybe LinkReference
        , _lEdict :: Maybe EdictReference
        }

data SpawnT =
  SpawnT { _spName  :: B.ByteString
         , _spSpawn :: EntThink
         }

data GameItemsGlobals =
  GameItemsGlobals { _giJacketArmorInfo      :: GItemArmorT
                   , _giCombatArmorInfo      :: GItemArmorT
                   , _giBodyArmorInfo        :: GItemArmorT
                   , _giQuakeDropTimeoutHack :: !Int
                   , _giJacketArmorIndex     :: GItemReference
                   , _giCombatArmorIndex     :: GItemReference
                   , _giBodyArmorIndex       :: GItemReference
                   , _giPowerScreenIndex     :: GItemReference
                   , _giPowerShieldIndex     :: GItemReference
                   }

data MSoldierGlobals =
  MSoldierGlobals { _msSoundIdle       :: !Int
                  , _msSoundSight1     :: !Int
                  , _msSoundSight2     :: !Int
                  , _msSoundPainLight  :: !Int
                  , _msSoundPain       :: !Int
                  , _msSoundPainSS     :: !Int
                  , _msSoundDeathLight :: !Int
                  , _msSoundDeath      :: !Int
                  , _msSoundDeathSS    :: !Int
                  , _msSoundCock       :: !Int
                  }

data MInfantryGlobals =
  MInfantryGlobals { _miSoundPain1      :: !Int
                   , _miSoundPain2      :: !Int
                   , _miSoundDie1       :: !Int
                   , _miSoundDie2       :: !Int
                   , _miSoundGunShot    :: !Int
                   , _miSoundWeaponCock :: !Int
                   , _miSoundPunchSwing :: !Int
                   , _miSoundPunchHit   :: !Int
                   , _miSoundSight      :: !Int
                   , _miSoundSearch     :: !Int
                   , _miSoundIdle       :: !Int
                   }

data MFrameT =
  MFrameT { _mfAI    :: Maybe AI
          , _mfDist  :: !Float
          , _mfThink :: Maybe EntThink
          }

data MMoveT =
  MMoveT { _mmId         :: B.ByteString -- to check for equality
         , _mmFirstFrame :: !Int
         , _mmLastFrame  :: !Int
         , _mmFrame      :: V.Vector MFrameT
         , _mmEndFunc    :: Maybe EntThink
         }

data PlayerTrailGlobals =
  PlayerTrailGlobals { _ptTrail       :: V.Vector EdictReference
                     , _ptTrailHead   :: !Int
                     , _ptTrailActive :: !Bool
                     }

data PushedT =
  PushedT { _pEnt      :: Maybe EdictReference
          , _pOrigin   :: V3 Float
          , _pAngles   :: V3 Float
          , _pDeltaYaw :: !Float
          }

data VIDGlobals =
  VIDGlobals { _vgVidModes           :: V.Vector VidModeT
             , _vgRefLibActive       :: !Bool
             , _vgFSModes            :: Maybe (V.Vector VidModeT)
             , _vgFSResolutions      :: V.Vector B.ByteString
             , _vgModeX              :: !Int
             , _vgRefs               :: V.Vector B.ByteString
             , _vgDrivers            :: V.Vector B.ByteString
             , _vgCurrentMenu        :: Maybe MenuFrameworkSReference
             }

data KBD =
  KBD { _kbdInit           :: Quake ()
      , _kbdUpdate         :: Quake ()
      , _kbdClose          :: Quake ()
      , _kbdDoKeyEvent     :: Int -> Bool -> Quake ()
      , _kbdInstallGrabs   :: Quake ()
      , _kbdUninstallGrabs :: Quake ()
      }

data Renderer = Renderer { _rName      :: B.ByteString
                         , _rRefExport :: RefExportT
                         }

data VideoMode =
  GLFWbVideoMode GLFW.VideoMode
-- | GLUTVideoMode GLUT.GameModeCapability

data GLDriver =
  GLDriver { _gldInit            :: Int -> Int -> Quake Bool
           , _gldSetMode         :: (Int, Int) -> Int -> Bool -> Quake Int
           , _gldShutdown        :: Quake ()
           , _gldBeginFrame      :: Float -> Quake ()
           , _gldEndFrame        :: Quake ()
           , _gldAppActivate     :: Bool -> Quake ()
           , _gldEnableLogging   :: Bool -> Quake ()
           , _gldLogNewFrame     :: Quake ()
           , _gldGetModeList     :: Quake (V.Vector VideoMode)
           , _gldUpdateScreen    :: XCommandT -> Quake ()
           , _gldSetSwapInterval :: Int -> Quake ()
           }

data RenderAPI =
  RenderAPI { _rInit              :: GLDriver -> Int -> Int -> Quake Bool
            , _rInit2             :: GLDriver -> Quake Bool
            , _rShutdown          :: GLDriver -> Quake ()
            , _rBeginRegistration :: GLDriver -> B.ByteString -> Quake ()
            , _rRegisterModel     :: GLDriver -> B.ByteString -> Quake (Maybe ModelReference)
            , _rRegisterSkin      :: GLDriver -> B.ByteString -> Quake (Maybe ImageReference)
            , _rDrawFindPic       :: GLDriver -> B.ByteString -> Quake (Maybe ImageReference)
            , _rSetSky            :: GLDriver -> B.ByteString -> Float -> V3 Float -> Quake ()
            , _rEndRegistration   :: GLDriver -> Quake ()
            , _rRenderFrame       :: GLDriver -> RefDefT -> Quake ()
            , _rDrawGetPicSize    :: GLDriver -> B.ByteString -> Quake (Maybe (Int, Int))
            , _rDrawPic           :: GLDriver -> Int -> Int -> B.ByteString -> Quake ()
            , _rDrawStretchPic    :: GLDriver -> Int -> Int -> Int -> Int -> B.ByteString -> Quake ()
            , _rDrawChar          :: GLDriver -> Int -> Int -> Int -> Quake ()
            , _rDrawTileClear     :: GLDriver -> Int -> Int -> Int -> Int -> B.ByteString -> Quake ()
            , _rDrawFill          :: GLDriver -> Int -> Int -> Int -> Int -> Int -> Quake ()
            , _rDrawFadeScreen    :: GLDriver -> Quake ()
            , _rDrawStretchRaw    :: GLDriver -> Int -> Int -> Int -> Int -> Int -> Int -> B.ByteString -> Quake ()
            , _rSetPalette        :: GLDriver -> Maybe B.ByteString -> Quake ()
            , _rBeginFrame        :: GLDriver -> Float -> Quake ()
            , _glScreenShotF      :: GLDriver -> XCommandT
            }

data INGlobals =
  INGlobals { _inMouseAvail          :: !Bool
            , _inMouseActive         :: !Bool
            , _inIgnoreFirst         :: !Bool
            , _inMouseButtonState    :: !Int
            , _inMouseOldButtonState :: !Int
            , _inOldMouseX           :: !Int
            , _inOldMouseY           :: !Int
            , _inMLooking            :: !Bool
            }

data GLFWKBDEvent =
    KeyPress
  | KeyRelease
  | MotionNotify
  | ButtonPress
  | ButtonRelease
  | ConfigureNotify
  | WheelMoved

data GLFWbGlobals =
  GLFWbGlobals { _glfwbOldDisplayMode :: Maybe GLFW.VideoMode
               , _glfwbWindow         :: Maybe GLFW.Window
               , _glfwbWindowXPos     :: !Int
               , _glfwbWindowYPos     :: !Int
               , _glfwbKBDChan        :: Maybe (TChan GLFWKBDEvent)
               }

data KBDGlobals =
  KBDGlobals { _kbdMx    :: !Int
             , _kbdMy    :: !Int
             , _kbdWinx  :: !Int
             , _kbdWiny  :: !Int
             }

data BasicRenderAPIGlobals =
  BasicRenderAPIGlobals

data FastRenderAPIGlobals =
  FastRenderAPIGlobals { _frGLDepthMin           :: !Float
                       , _frGLDepthMax           :: !Float
                       , _frGLConfig             :: GLConfigT
                       , _frGLState              :: GLStateT
                       , _frd8to24table          :: UV.Vector Int
                       , _frVid                  :: VidDefT
                       , _frColorTableEXT        :: !Bool
                       , _frActiveTextureARB     :: !Bool
                       , _frPointParameterEXT    :: !Bool
                       , _frLockArraysEXT        :: !Bool
                       , _frSwapIntervalEXT      :: !Bool
                       , _frTexture0             :: !Int
                       , _frTexture1             :: !Int
                       , _frGLTexSolidFormat     :: !Int
                       , _frGLTexAlphaFormat     :: !Int
                       , _frGLFilterMin          :: !Int
                       , _frGLFilterMax          :: !Int
                       , _frNumGLTextures        :: !Int
                       , _frGLTextures           :: V.Vector ImageT
                       , _frLastModes            :: (Int, Int)
                       , _frRegistrationSequence :: !Int
                       , _frGammaTable           :: B.ByteString
                       , _frIntensityTable       :: B.ByteString
                       , _frModKnown             :: V.Vector ModelT
                       , _frModNumKnown          :: !Int
                       , _frLoadModel            :: ModelReference
                       , _frCurrentModel         :: ModelReference
                       , _frModInline            :: V.Vector ModelT
                       , _frModNoVis             :: B.ByteString
                       , _frNoTexture            :: ImageReference
                       , _frParticleTexture      :: ImageReference
                       , _frUploadWidth          :: !Int
                       , _frUploadHeight         :: !Int
                       , _frUploadedPaletted     :: !Bool
                       , _frDrawChars            :: Maybe ImageReference
                       , _frTrickFrame           :: !Int
                       , _frScrapDirty           :: !Bool
                       , _frViewCluster          :: !Int
                       , _frViewCluster2         :: !Int
                       , _frOldViewCluster       :: !Int
                       , _frOldViewCluster2      :: !Int
                       , _frWorldModel           :: Maybe ModelReference
                       , _frModelTextureCoordBuf :: MSV.IOVector Float
                       , _frModelVertexIndexBuf  :: MSV.IOVector Int32
                       , _frModelTextureCoordIdx :: !Int
                       , _frModelVertexIndexIdx  :: !Int
                       , _frPolygonS1Old         :: UV.Vector Float
                       , _frPolygonBuffer        :: MSV.IOVector Float
                       , _frPolygonCache         :: MV.IOVector GLPolyT
                       , _frPolygonBufferIndex   :: !Int
                       , _frPolygonCount         :: !Int
                       , _frGLLms                :: GLLightMapStateT
                       , _frNewRefDef            :: RefDefT
                       , _frFrameCount           :: !Int
                       , _frWarpFace             :: Maybe MSurfaceT
                       , _frModelVisibility      :: B.ByteString
                       , _frSkyName              :: B.ByteString
                       , _frSkyRotate            :: !Float
                       , _frSkyAxis              :: V3 Float
                       , _frSkyImages            :: V.Vector (Maybe ImageReference)
                       , _frSkyMin               :: !Float
                       , _frSkyMax               :: !Float
                       }

data ParticleTGlobals =
  ParticleTGlobals { _pColorTable :: UV.Vector Int
                   }

data MenuFrameworkS =
  MenuFrameworkS { _mfX          :: !Int
                 , _mfY          :: !Int
                 , _mfCursor     :: !Int
                 , _mfNItems     :: !Int
                 , _mfNSlots     :: !Int
                 , _mfItems      :: V.Vector (Maybe MenuItemReference)
                 , _mfStatusBar  :: B.ByteString
                 , _mfCursorDraw :: Maybe (Quake ())
                 }

data MenuCommonS =
  MenuCommonS { _mcType          :: !Int
              , _mcName          :: B.ByteString
              , _mcX             :: !Int
              , _mcY             :: !Int
              , _mcParent        :: Maybe MenuFrameworkSReference
              , _mcCursorOffset  :: !Int
              , _mcLocalData     :: V4 Int
              , _mcFlags         :: !Int
              , _mcN             :: !Int
              , _mcStatusBar     :: B.ByteString
              , _mcCallback      :: Maybe (Quake ())
              , _mcStatusBarFunc :: Maybe (Quake ())
              , _mcOwnerDraw     :: Maybe (Quake ())
              , _mcCursorDraw    :: Maybe (Quake ())
              }

data MenuItem =
    MenuListS { _mlGeneric   :: MenuCommonS
              , _mlCurValue  :: !Int
              , _mlItemNames :: Maybe (V.Vector B.ByteString)
              }
  | MenuSliderS { _msGeneric  :: MenuCommonS
                , _msMinValue :: !Float
                , _msMaxValue :: !Float
                , _msCurValue :: !Float
                , _msRange    :: !Float
                }
  | MenuActionS { _maGeneric :: MenuCommonS
                }

data MenuGlobals =
  MenuGlobals { _mgMenuFrameworks :: V.Vector MenuFrameworkS
              , _mgMenuItems      :: V.Vector MenuItem
              , _mgLayers         :: V.Vector MenuLayerT
              , _mgDrawFunc       :: Maybe XCommandT
              , _mgEnterSound     :: !Bool
              }

data MenuLayerT =
  MenuLayerT { _mlDraw :: Maybe XCommandT
             , _mlKey  :: Maybe KeyFuncT
             }

data ClientGlobals =
  ClientGlobals { _cgExtraTime          :: !Int
                , _cgNumCheatVars       :: !Int
                , _cgBuf                :: SizeBufT
                , _cgFrameMsec          :: !Int64
                , _cgOldSysFrameTime    :: !Int64
                , _cgInKLook            :: KButtonT
                , _cgInLeft             :: KButtonT
                , _cgInRight            :: KButtonT
                , _cgInForward          :: KButtonT
                , _cgInBack             :: KButtonT
                , _cgInLookUp           :: KButtonT
                , _cgInLookDown         :: KButtonT
                , _cgInMoveLeft         :: KButtonT
                , _cgInMoveRight        :: KButtonT
                , _cgInStrafe           :: KButtonT
                , _cgInSpeed            :: KButtonT
                , _cgInUse              :: KButtonT
                , _cgInAttack           :: KButtonT
                , _cgInUp               :: KButtonT
                , _cgInDown             :: KButtonT
                , _cgInImpulse          :: !Int
                , _cgDLights            :: V.Vector CDLightT
                , _cgLightStyle         :: V.Vector CLightStyleT
                , _cgLastOfs            :: !Int
                , _cgCR                 :: !Int -- from Console.hs
                , _cgParticles          :: V.Vector CParticleT
                , _cgActiveParticles    :: Maybe CParticleReference
                , _cgFreeParticles      :: CParticleReference
                , _cgPrecacheCheck      :: !Int
                , _cgPrecacheSpawnCount :: !Int
                , _cgPrecacheTex        :: !Int
                , _cgPrecacheModelSkin  :: !Int
                , _cgPrecacheModel      :: Maybe B.ByteString
                , _cgNumCLWeaponModels  :: !Int
                , _cgWeaponModels       :: V.Vector B.ByteString
                }

data VRectT =
  VRectT { _vrX      :: !Int
         , _vrY      :: !Int
         , _vrWidth  :: !Int
         , _vrHeight :: !Int
         }

data VGlobals =
  VGlobals { _vgRNumDLights   :: !Int
           , _vgRNumEntities  :: !Int
           , _vgRNumParticles :: !Int
           , _vgRLightStyles  :: V.Vector LightStyleT
           , _vgREntities     :: V.Vector EntityT
           , _vgRDLights      :: V.Vector DLightT
           }

data CDLightT =
  CDLightT { _cdlKey      :: !Int
           , _cdlColor    :: V3 Float
           , _cdlOrigin   :: V3 Float
           , _cdlRadius   :: !Float
           , _cdlDie      :: !Float
           , _cdlMinLight :: !Float
           }

data CLightStyleT =
  CLightStyleT { _clsLength :: !Int
               , _clsValue  :: V3 Float
               , _clsMap    :: UV.Vector Float
               }

data CinematicsT =
  CinematicsT { _cRestartSound :: !Bool
              , _cSRate        :: !Int
              , _cSWidth       :: !Int
              , _cSChannels    :: !Int
              , _cWidth        :: !Int
              , _cHeight       :: !Int
              , _cPic          :: Maybe B.ByteString
              , _cPicPending   :: Maybe B.ByteString
              , _cHNodes1      :: Maybe (UV.Vector Int)
              , _cNumHNodes1   :: UV.Vector Int
              , _cHUsed        :: UV.Vector Int
              , _cHCount       :: UV.Vector Int
              }

data NetChannelGlobals =
  NetChannelGlobals { _ncSendBuf :: B.ByteString
                    , _ncSend    :: SizeBufT
                    }

data CParticleT =
  CParticleT { _cpTime     :: !Float
             , _cpOrg      :: V3 Float
             , _cpVel      :: V3 Float
             , _cpAccel    :: V3 Float
             , _cpColor    :: !Float
             , _cpAlpha    :: !Float
             , _cpAlphaVel :: !Float
             , _cpNext     :: Maybe CParticleReference
             }

data GLLightMapStateT =
  GLLightMapStateT { _lmsInternalFormat         :: !Int
                   , _lmsCurrentLightmapTexture :: !Int
                   , _lmsLightmapSurfaces       :: V.Vector Int -- TODO: reference ?
                   , _lmsAllocated              :: UV.Vector Int
                   , _lmsLightmapBuffer         :: MSV.IOVector Word8
                   }

data MNodeT =
  MNodeT { _mnContents     :: !Int
         , _mnVisFrame     :: !Int
         , _mnMins         :: V3 Float
         , _mnMaxs         :: V3 Float
         , _mnParent       :: Maybe MNodeReference
         , _mnPlane        :: CPlaneT
         , _mnChildren     :: (MNodeChild, MNodeChild)
         , _mnFirstSurface :: !Int
         , _mnNumSurfaces  :: !Int
         }

data BeamT =
  BeamT { _bEntity     :: !Int
        , _bDestEntity :: !Int
        , _bModel      :: Maybe ModelReference
        , _bEndTime    :: !Int
        , _bOffset     :: V3 Float
        , _bStart      :: V3 Float
        , _bEnd        :: V3 Float
        }

data CLSustainT =
  CLSustainT { _clsId            :: !Int
             , _clsType          :: !Int
             , _clsEndTime       :: !Int
             , _clsNextThink     :: !Int
             , _clsThinkInterval :: !Int
             , _clsOrg           :: V3 Float
             , _clsDir           :: V3 Float
             , _clsColor         :: !Int
             , _clsCount         :: !Int
             , _clsMagnitude     :: !Int
             , _clsThink         :: Maybe EntThink
             }

data LaserT =
  LaserT { _lEnt     :: EntityT
         , _lEndTime :: !Int
         }

data ExplosionT =
  ExplosionT { _eType       :: !Int
             , _eEnt        :: EntityT
             , _eFrames     :: !Int
             , _eLight      :: Float
             , _eLightColor :: V3 Float
             , _eStart      :: Float
             , _eBaseFrame  :: !Int
             }

data CLTEntGlobals =
  CLTEntGlobals { _clteExplosions         :: V.Vector ExplosionT
                , _clteBeams              :: V.Vector BeamT
                , _cltePlayerBeams        :: V.Vector BeamT
                , _clteLasers             :: V.Vector LaserT
                , _clteSustains           :: V.Vector CLSustainT
                , _clteSfxRic1            :: Maybe SfxReference
                , _clteSfxRic2            :: Maybe SfxReference
                , _clteSfxRic3            :: Maybe SfxReference
                , _clteSfxLashIt          :: Maybe SfxReference
                , _clteSfxSpark5          :: Maybe SfxReference
                , _clteSfxSpark6          :: Maybe SfxReference
                , _clteSfxSpark7          :: Maybe SfxReference
                , _clteSfxRailg           :: Maybe SfxReference
                , _clteSfxRockExp         :: Maybe SfxReference
                , _clteSfxGrenExp         :: Maybe SfxReference
                , _clteSfxWatrExp         :: Maybe SfxReference
                , _clteSfxPlasExp         :: Maybe SfxReference
                , _clteSfxFootsteps       :: V.Vector SfxT
                , _clteModExplode         :: Maybe ModelReference
                , _clteModSmoke           :: Maybe ModelReference
                , _clteModFlash           :: Maybe ModelReference
                , _clteModParasiteSegment :: Maybe ModelReference
                , _clteModGrappleCable    :: Maybe ModelReference
                , _clteModParasiteTip     :: Maybe ModelReference
                , _clteModExplo4          :: Maybe ModelReference
                , _clteModBfgExplo        :: Maybe ModelReference
                , _clteModPowerScreen     :: Maybe ModelReference
                , _clteModPlasmaExplo     :: Maybe ModelReference
                , _clteSfxLightning       :: Maybe SfxReference
                , _clteSfxDisrExp         :: Maybe SfxReference
                , _clteModLightning       :: Maybe ModelReference
                , _clteModHeatBeam        :: Maybe ModelReference
                , _clteModMonsterHeatBeam :: Maybe ModelReference
                , _clteModExplo4Big       :: Maybe ModelReference
                }

data MBoss31Globals =
  MBoss31Globals { _mb31SoundPain1     :: Int
                 , _mb31SoundPain2     :: Int
                 , _mb31SoundPain3     :: Int
                 , _mb31SoundIdle      :: Int
                 , _mb31SoundDeath     :: Int
                 , _mb31SoundSearch1   :: Int
                 , _mb31SoundSearch2   :: Int
                 , _mb31SoundSearch3   :: Int
                 , _mb31SoundAttack1   :: Int
                 , _mb31SoundAttack2   :: Int
                 , _mb31SoundFireGun   :: Int
                 , _mb31SoundStepLeft  :: Int
                 , _mb31SoundStepRight :: Int
                 , _mb31SoundDeathHit  :: Int
                 }

data MBoss2Globals =
  MBoss2Globals { _mb2SoundPain1   :: Int
                , _mb2SoundPain2   :: Int
                , _mb2SoundPain3   :: Int
                , _mb2SoundDeath   :: Int
                , _mb2SoundSearch1 :: Int
                }

data MBerserkGlobals =
  MBerserkGlobals { _mBerserkSoundPain   :: Int
                  , _mBerserkSoundDie    :: Int
                  , _mBerserkSoundIdle   :: Int
                  , _mBerserkSoundPunch  :: Int
                  , _mBerserkSoundSight  :: Int
                  , _mBerserkSoundSearch :: Int
                  }

data MBoss32Globals =
  MBoss32Globals { _mb32SoundPain4        :: Int
                 , _mb32SoundPain5        :: Int
                 , _mb32SoundPain6        :: Int
                 , _mb32SoundDeath        :: Int
                 , _mb32SoundStepLeft     :: Int
                 , _mb32SoundStepRight    :: Int
                 , _mb32SoundAttackBfg    :: Int
                 , _mb32SoundBrainSplorch :: Int
                 , _mb32SoundPreRailGun   :: Int
                 , _mb32SoundPopUp        :: Int
                 , _mb32SoundTaunt1       :: Int
                 , _mb32SoundTaunt2       :: Int
                 , _mb32SoundTaunt3       :: Int
                 , _mb32SoundHit          :: Int
                 }

data MBrainGlobals =
  MBrainGlobals { _mBrainSoundChestOpen        :: Int
                , _mBrainSoundTentaclesExtend  :: Int
                , _mBrainSoundTentaclesRetract :: Int
                , _mBrainSoundDeath            :: Int
                , _mBrainSoundIdle1            :: Int
                , _mBrainSoundIdle2            :: Int
                , _mBrainSoundIdle3            :: Int
                , _mBrainSoundPain1            :: Int
                , _mBrainSoundPain2            :: Int
                , _mBrainSoundSight            :: Int
                , _mBrainSoundSearch           :: Int
                , _mBrainSoundMelee1           :: Int
                , _mBrainSoundMelee2           :: Int
                , _mBrainSoundMelee3           :: Int
                }

data MChickGlobals =
  MChickGlobals { _mChickSoundMissilePrelaunch :: Int
                , _mChickSoundMissileLaunch    :: Int
                , _mChickSoundMeleeSwing       :: Int
                , _mChickSoundMeleeHit         :: Int
                , _mChickSoundMissileReload    :: Int
                , _mChickSoundDeath1           :: Int
                , _mChickSoundDeath2           :: Int
                , _mChickSoundFallDown         :: Int
                , _mChickSoundIdle1            :: Int
                , _mChickSoundIdle2            :: Int
                , _mChickSoundPain1            :: Int
                , _mChickSoundPain2            :: Int
                , _mChickSoundPain3            :: Int
                , _mChickSoundSight            :: Int
                , _mChickSoundSearch           :: Int
                }

data MFlipperGlobals =
  MFlipperGlobals { _mFlipperSoundChomp  :: Int
                  , _mFlipperSoundAttack :: Int
                  , _mFlipperSoundPain1  :: Int
                  , _mFlipperSoundPain2  :: Int
                  , _mFlipperSoundDeath  :: Int
                  , _mFlipperSoundIdle   :: Int
                  , _mFlipperSoundSearch :: Int
                  , _mFlipperSoundSight  :: Int
                  }

data MFloatGlobals =
  MFloatGlobals { _mFloatSoundAttack2 :: Int
                , _mFloatSoundAttack3 :: Int
                , _mFloatSoundDeath1  :: Int
                , _mFloatSoundIdle    :: Int
                , _mFloatSoundPain1   :: Int
                , _mFloatSoundPain2   :: Int
                , _mFloatSoundSight   :: Int
                }

data MFlyerGlobals =
  MFlyerGlobals { _mFlyerNextMove     :: Int
                , _mFlyerSoundSight   :: Int
                , _mFlyerSoundIdle    :: Int
                , _mFlyerSoundPain1   :: Int
                , _mFlyerSoundPain2   :: Int
                , _mFlyerSoundSlash   :: Int
                , _mFlyerSoundSproing :: Int
                , _mFlyerSoundDie     :: Int
                }

data MGladiatorGlobals =
  MGladiatorGlobals { _mGladiatorSoundPain1        :: Int
                    , _mGladiatorSoundPain2        :: Int
                    , _mGladiatorSoundDie          :: Int
                    , _mGladiatorSoundGun          :: Int
                    , _mGladiatorSoundCleaverSwing :: Int
                    , _mGladiatorSoundCleaverHit   :: Int
                    , _mGladiatorSoundCleaverMiss  :: Int
                    , _mGladiatorSoundIdle         :: Int
                    , _mGladiatorSoundSearch       :: Int
                    , _mGladiatorSoundSight        :: Int
                    }

data MGunnerGlobals =
  MGunnerGlobals { _mGunnerSoundPain   :: Int
                 , _mGunnerSoundPain2  :: Int
                 , _mGunnerSoundDeath  :: Int
                 , _mGunnerSoundIdle   :: Int
                 , _mGunnerSoundOpen   :: Int
                 , _mGunnerSoundSearch :: Int
                 , _mGunnerSoundSight  :: Int
                 }

data MHoverGlobals =
  MHoverGlobals { _mHoverSoundPain1   :: Int
                , _mHoverSoundPain2   :: Int
                , _mHoverSoundDeath1  :: Int
                , _mHoverSoundDeath2  :: Int
                , _mHoverSoundSight   :: Int
                , _mHoverSoundSearch1 :: Int
                , _mHoverSoundSearch2 :: Int
                }

data MInsaneGlobals =
  MInsaneGlobals { _mInsaneSoundFist   :: Int
                 , _mInsaneSoundShake  :: Int
                 , _mInsaneSoundMoan   :: Int
                 , _mInsaneSoundScream :: UV.Vector Int
                 }

data MMedicGlobals =
  MMedicGlobals { _mMedicSoundIdle1       :: Int
                , _mMedicSoundPain1       :: Int
                , _mMedicSoundPain2       :: Int
                , _mMedicSoundDie         :: Int
                , _mMedicSoundSight       :: Int
                , _mMedicSoundSearch      :: Int
                , _mMedicSoundHookLaunch  :: Int
                , _mMedicSoundHookHit     :: Int
                , _mMedicSoundHookHeal    :: Int
                , _mMedicSoundHookRetract :: Int
                }

data MMutantGlobals =
  MMutantGlobals { _mMutantSoundSwing  :: Int
                 , _mMutantSoundHit    :: Int
                 , _mMutantSoundHit2   :: Int
                 , _mMutantSoundDeath  :: Int
                 , _mMutantSoundIdle   :: Int
                 , _mMutantSoundPain1  :: Int
                 , _mMutantSoundPain2  :: Int
                 , _mMutantSoundSight  :: Int
                 , _mMutantSoundSearch :: Int
                 , _mMutantSoundStep1  :: Int
                 , _mMutantSoundStep2  :: Int
                 , _mMutantSoundStep3  :: Int
                 , _mMutantSoundThud   :: Int
                 }
