{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ImpredicativeTypes #-}
module Types where

import Control.Concurrent.STM.TChan (TChan)
import Control.Lens (Zoom, zoom, Lens')
import Control.Lens.Internal.Zoom (Zoomed, Focusing)
import Control.Monad.Except
import Control.Monad.State (StateT, MonadState, MonadIO)
import Control.Monad.State (liftIO)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.IORef (IORef)
import Data.Sequence (Seq)
import Data.Word (Word8, Word16, Word32)
import Linear (V3, V4)
import Network.Socket (HostAddress)
import System.IO (Handle)
import System.Random (StdGen)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import qualified Data.HashMap.Lazy as HM
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Storable.Mutable as MSV
import qualified Data.Vector.Unboxed as UV
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.GL as GL

io :: MonadIO m => IO a -> m a
io = liftIO

whenM :: Quake Bool -> Quake () -> Quake ()
whenM q f = q >>= \b -> when b f

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

data XCommandT =
  XCommandT { _xcName :: B.ByteString
            , _xcCmd  :: Quake ()
            }

instance Eq XCommandT where
    x == y = _xcName x == _xcName y

data KeyFuncT =
  KeyFuncT { _kfName :: B.ByteString
           , _kfFunc :: Int -> Quake (Maybe B.ByteString)
           }

instance Eq KeyFuncT where
    x == y = _kfName x == _kfName y

data Ref a = Ref Int deriving (Eq, Show, Ord)

-- reference to gameBaseGlobals.gbItemList
newtype GItemReference = GItemReference Int deriving Eq

data MenuItemRef = MenuListRef (Ref MenuListS)
                 | MenuActionRef (Ref MenuActionS)
                 | MenuSliderRef (Ref MenuSliderS)
                 | MenuSeparatorRef (Ref MenuSeparatorS)
                 | MenuFieldRef (Ref MenuFieldS)
                 deriving Eq

-- reference to globals.cl.cmds
newtype UserCmdReference = UserCmdReference Int deriving Eq

newtype MTexInfoReference = MTexInfoReference Int deriving Eq

newtype GLPolyReference = GLPolyReference Int deriving Eq

newtype MNodeReference = MNodeReference Int deriving Eq

newtype SfxReference = SfxReference Int deriving Eq

data MNodeChild = MNodeChildReference (IORef MNodeT) | MLeafChildReference (IORef MLeafT)

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
             , _mParasiteGlobals      :: MParasiteGlobals
             , _mSoldierGlobals       :: MSoldierGlobals
             , _mSuperTankGlobals     :: MSuperTankGlobals
             , _mTankGlobals          :: MTankGlobals
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
  Globals { _gCurTime           :: IORef Int
          , _gCmdWait            :: Bool
          , _gAliasCount         :: Int
          , _gCTraces            :: Int
          , _gCBrushTraces       :: Int
          , _gCPointContents     :: Int
          , _gServerState        :: Int
          , _gNetMessage         :: SizeBufT
          , _gNetMessageBuffer   :: B.ByteString
          , _gCmdText            :: SizeBufT
          , _gDeferTextBuf       :: B.ByteString -- length 8192
          , _gCmdTextBuf         :: B.ByteString -- length 8192
          , _gCmdAlias           :: Seq CmdAliasT
          , _gTimeBeforeGame     :: Int
          , _gTimeAfterGame      :: Int
          , _gTimeBeforeRef      :: Int
          , _gTimeAfterRef       :: Int
          , _gLogStatsFile       :: Maybe Handle
          , _gCls                :: ClientStaticT
          , _gCl                 :: ClientStateT
          , _gClEntities         :: V.Vector CEntityT
          , _gClParseEntities    :: V.Vector EntityStateT
          , _gUserInfoModified   :: Bool
          , _gCVars              :: HM.HashMap B.ByteString CVarT
          , _gCon                :: ConsoleT
          , _gVidDef             :: VidDefT
          , _gRenderer           :: Maybe Renderer
          , _gKeyBindings        :: V.Vector (Maybe B.ByteString)
          , _gKeyDown            :: UV.Vector Bool
          , _gChatTeam           :: Bool
          , _gChatBuffer         :: B.ByteString
          , _gKeyLines           :: V.Vector B.ByteString
          , _gKeyLinePos         :: Int
          , _gEditLine           :: Int
          , _gScrVRect           :: VRectT
          , _gSysFrameTime       :: Int
          , _gChatBufferLen      :: Int
          , _gGunFrame           :: Int
          , _gGunModel           :: Maybe (IORef ModelT)
          , _gNetFrom            :: NetAdrT
          , _gLogFile            :: Maybe Handle
          , _gVec3Origin         :: V3 Float
          , _gRnd               :: StdGen
          }

data ComGlobals =
  ComGlobals { _cgComArgc       :: Int
             , _cgComArgv       :: V.Vector B.ByteString
             , _cgRecursive     :: Bool
             , _cgMsg           :: B.ByteString
             , _cgDebugContext  :: B.ByteString
             , _cgDebugContext2 :: B.ByteString
             , _cgRdTarget      :: Int
             }

data CmdGlobals =
  CmdGlobals { _cgCmdFunctions :: Seq CmdFunctionT
             , _cgCmdArgc      :: Int
             , _cgCmdArgv      :: V.Vector B.ByteString
             , _cgCmdArgs      :: B.ByteString
             }

data KeyGlobals =
  KeyGlobals { _kgAnyKeyDown  :: Int
             , _kgKeyWaiting  :: Int
             , _kgHistoryLine :: Int
             , _kgShiftDown   :: Bool
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
            , _fsFileFromPak     :: Int
            }

data SVGlobals =
  SVGlobals { _svMasterAdr            :: V.Vector NetAdrT
            , _svClient               :: Maybe (Ref ClientT)
            , _svServer               :: ServerT
            , _svServerStatic         :: ServerStaticT
            , _svPlayer               :: Maybe (Ref EdictT)
            , _svFirstMap             :: B.ByteString
            , _svMsgBuf               :: B.ByteString
            , _svNumAreaNodes         :: Int
            , _svAreaNodes            :: V.Vector AreaNodeT
            , _svAreaMins             :: V3 Float
            , _svAreaMaxs             :: V3 Float
            , _svAreaList             :: V.Vector (Ref EdictT)
            , _svAreaCount            :: Int
            , _svAreaMaxCount         :: Int
            , _svAreaType             :: Int
            , _svLeafs                :: UV.Vector Int
            , _svClusters             :: UV.Vector Int
            , _svTouch                :: V.Vector (Ref EdictT)
            , _svTouchList            :: V.Vector (Ref EdictT)
            , _svLinks                :: V.Vector LinkT
            , _svMsg                  :: SizeBufT
            , _svLeafsTmp             :: UV.Vector Int
            , _svFatPVS               :: UV.Vector Word8
            }

data CLTEntGlobals =
  CLTEntGlobals { _clteExplosions         :: V.Vector (IORef ExplosionT)
                , _clteBeams              :: V.Vector BeamT
                , _cltePlayerBeams        :: V.Vector BeamT
                , _clteLasers             :: V.Vector LaserT
                , _clteSustains           :: V.Vector CLSustainT
                , _clteSfxRic1            :: Maybe (IORef SfxT)
                , _clteSfxRic2            :: Maybe (IORef SfxT)
                , _clteSfxRic3            :: Maybe (IORef SfxT)
                , _clteSfxLashIt          :: Maybe (IORef SfxT)
                , _clteSfxSpark5          :: Maybe (IORef SfxT)
                , _clteSfxSpark6          :: Maybe (IORef SfxT)
                , _clteSfxSpark7          :: Maybe (IORef SfxT)
                , _clteSfxRailg           :: Maybe (IORef SfxT)
                , _clteSfxRockExp         :: Maybe (IORef SfxT)
                , _clteSfxGrenExp         :: Maybe (IORef SfxT)
                , _clteSfxWatrExp         :: Maybe (IORef SfxT)
                , _clteSfxPlasExp         :: Maybe (IORef SfxT)
                , _clteSfxFootsteps       :: V.Vector (Maybe (IORef SfxT))
                , _clteModExplode         :: Maybe (IORef ModelT)
                , _clteModSmoke           :: Maybe (IORef ModelT)
                , _clteModFlash           :: Maybe (IORef ModelT)
                , _clteModParasiteSegment :: Maybe (IORef ModelT)
                , _clteModGrappleCable    :: Maybe (IORef ModelT)
                , _clteModParasiteTip     :: Maybe (IORef ModelT)
                , _clteModExplo4          :: Maybe (IORef ModelT)
                , _clteModBfgExplo        :: Maybe (IORef ModelT)
                , _clteModPowerScreen     :: Maybe (IORef ModelT)
                , _clteModPlasmaExplo     :: Maybe (IORef ModelT)
                , _clteSfxLightning       :: Maybe (IORef SfxT)
                , _clteSfxDisrExp         :: Maybe (IORef SfxT)
                , _clteModLightning       :: Maybe (IORef ModelT)
                , _clteModHeatBeam        :: Maybe (IORef ModelT)
                , _clteModMonsterHeatBeam :: Maybe (IORef ModelT)
                , _clteModExplo4Big       :: Maybe (IORef ModelT)
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

data MParasiteGlobals =
  MParasiteGlobals { _mParasiteSoundPain1   :: Int
                   , _mParasiteSoundPain2   :: Int
                   , _mParasiteSoundDie     :: Int
                   , _mParasiteSoundLaunch  :: Int
                   , _mParasiteSoundImpact  :: Int
                   , _mParasiteSoundSuck    :: Int
                   , _mParasiteSoundReelIn  :: Int
                   , _mParasiteSoundSight   :: Int
                   , _mParasiteSoundTap     :: Int
                   , _mParasiteSoundScratch :: Int
                   , _mParasiteSoundSearch  :: Int
                   }

data MSuperTankGlobals =
  MSuperTankGlobals { _mSuperTankSoundPain1   :: Int
                    , _mSuperTankSoundPain2   :: Int
                    , _mSuperTankSoundPain3   :: Int
                    , _mSuperTankSoundDeath   :: Int
                    , _mSuperTankSoundSearch1 :: Int
                    , _mSuperTankSoundSearch2 :: Int
                    , _mSuperTankTreadSound   :: Int
                    }

data MTankGlobals =
  MTankGlobals { _mTankSoundThud   :: Int
               , _mTankSoundPain   :: Int
               , _mTankSoundIdle   :: Int
               , _mTankSoundDie    :: Int
               , _mTankSoundStep   :: Int
               , _mTankSoundSight  :: Int
               , _mTankSoundWindUp :: Int
               , _mTankSoundStrike :: Int
               }


data CmdFunctionT =
  CmdFunctionT { _cfName     :: B.ByteString
               , _cfFunction :: Maybe XCommandT
               }

data EdictT =
  EdictT { _eEntityState           :: EntityStateT
         , _eInUse                 :: Bool
         , _eClassName             :: B.ByteString
         , _eLinkCount             :: Int
         , _eArea                  :: Ref LinkT
         , _eNumClusters           :: Int
         , _eClusterNums           :: UV.Vector Int
         , _eHeadNode              :: Int
         , _eAreaNum               :: Int
         , _eAreaNum2              :: Int
         , _eSvFlags               :: Int
         , _eSolid                 :: Int
         , _eClipMask              :: Int
         , _eMoveType              :: Int
         , _eFlags                 :: Int
         , _eFreeTime              :: Float
         , _eSpawnFlags            :: Int
         , _eTimeStamp             :: Float
         , _eAngle                 :: Float
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
         , _eYawSpeed              :: Float
         , _eIdealYaw              :: Float
         , _eTargetEnt             :: Maybe (Ref EdictT)
         , _eGoalEntity            :: Maybe (Ref EdictT)
         , _eMoveTarget            :: Maybe (Ref EdictT)
         , _eNextThink             :: Float
         , _ePrethink              :: Maybe EntThink
         , _eThink                 :: Maybe EntThink
         , _eBlocked               :: Maybe EntBlocked
         , _eTouch                 :: Maybe EntTouch
         , _eUse                   :: Maybe EntUse
         , _ePain                  :: Maybe EntPain
         , _eDie                   :: Maybe EntDie
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
         , _eViewHeight            :: Int
         , _eTakeDamage            :: Int
         , _eDmg                   :: Int
         , _eRadiusDmg             :: Int
         , _eDmgRadius             :: Float
         , _eSounds                :: Int
         , _eCount                 :: Int
         , _eGroundEntityLinkCount :: Int
         , _eChain                 :: Maybe (Ref EdictT)
         , _eEnemy                 :: Maybe (Ref EdictT)
         , _eOldEnemy              :: Maybe (Ref EdictT)
         , _eActivator             :: Maybe (Ref EdictT)
         , _eGroundEntity          :: Maybe (Ref EdictT)
         , _eTeamChain             :: Maybe (Ref EdictT)
         , _eTeamMaster            :: Maybe (Ref EdictT)
         , _eMyNoise               :: Maybe (Ref EdictT)
         , _eMyNoise2              :: Maybe (Ref EdictT)
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
         , _eItem                  :: Maybe GItemReference
         , _eMoveInfo              :: MoveInfoT
         , _eMonsterInfo           :: MonsterInfoT
         , _eClient                :: Maybe (Ref GClientT)
         , _eOwner                 :: Maybe (Ref EdictT)
         , _eIndex                 :: Int
         , _eiModel                :: Maybe B.ByteString
         , _eMessage               :: Maybe B.ByteString
         , _eTarget                :: Maybe B.ByteString
         , _eTargetName            :: Maybe B.ByteString
         , _eKillTarget            :: Maybe B.ByteString
         , _eTeam                  :: Maybe B.ByteString
         , _ePathTarget            :: Maybe B.ByteString
         , _eDeathTarget           :: Maybe B.ByteString
         , _eCombatTarget          :: Maybe B.ByteString
         , _eMap                   :: Maybe B.ByteString
         , _eMins                  :: V3 Float
         , _eMaxs                  :: V3 Float
         , _eAbsMin                :: V3 Float
         , _eAbsMax                :: V3 Float
         , _eSize                  :: V3 Float
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
               , _esSurroundingEnt :: Maybe (Ref EdictT)
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
           , _gcNewWeapon          :: Maybe GItemReference
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
           , _gcChaseTarget        :: Maybe (Ref EdictT)
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
          , _cEdict         :: Maybe (Ref EdictT)
          , _cName          :: B.ByteString
          , _cMessageLevel  :: Int
          , _cDatagram      :: SizeBufT
          , _cDatagramBuf   :: B.ByteString
          , _cFrames        :: V.Vector ClientFrameT
          , _cDownload      :: Maybe B.ByteString
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
          , _sModels        :: V.Vector (Ref CModelT)
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
               , _llSightClient          :: Maybe (Ref EdictT)
               , _llSightEntity          :: Maybe (Ref EdictT)
               , _llSightEntityFrameNum  :: Int
               , _llSoundEntity          :: Maybe (Ref EdictT)
               , _llSoundEntityFrameNum  :: Int
               , _llSound2Entity         :: Maybe (Ref EdictT)
               , _llSound2EntityFrameNum :: Int
               , _llPicHealth            :: Int
               , _llTotalSecrets         :: Int
               , _llFoundSecrets         :: Int
               , _llTotalGoals           :: Int
               , _llFoundGoals           :: Int
               , _llTotalMonsters        :: Int
               , _llKilledMonsters       :: Int
               , _llCurrentEntity        :: Maybe (Ref EdictT)
               , _llBodyQue              :: Int
               , _llPowerCubes           :: Int
               }

data GameImportT =
  GameImportT { _giBprintf            :: Int -> B.ByteString -> Quake ()
              , _giDprintf            :: B.ByteString -> Quake ()
              , _giCprintf            :: Maybe (Ref EdictT) -> Int -> B.ByteString -> Quake ()
              , _giCenterPrintf       :: Ref EdictT -> B.ByteString -> Quake ()
              , _giSound              :: Maybe (Ref EdictT) -> Int -> Int -> Float -> Float -> Float -> Quake ()
              , _giPositionedSound    :: Maybe (V3 Float) -> (Ref EdictT) -> Int -> Int -> Float -> Float -> Float -> Quake ()
              , _giConfigString       :: Int -> B.ByteString -> Quake ()
              , _giError              :: B.ByteString -> Quake ()
              , _giError2             :: Int -> B.ByteString -> Quake ()
              , _giModelIndex         :: Maybe B.ByteString -> Quake Int
              , _giSoundIndex         :: Maybe B.ByteString -> Quake Int
              , _giImageIndex         :: Maybe B.ByteString -> Quake Int
              , _giSetModel           :: (Ref EdictT) -> Maybe B.ByteString -> Quake ()
              , _giTrace              :: V3 Float -> Maybe (V3 Float) -> Maybe (V3 Float) -> V3 Float -> Maybe (Ref EdictT) -> Int -> Quake TraceT
              , _giPointContents      :: V3 Float -> Quake Int
              , _giInPHS              :: V3 Float -> V3 Float -> Quake Bool
              , _giSetAreaPortalState :: Int -> Bool -> Quake ()
              , _giAreasConnected     :: Int -> Int -> Quake Bool
              , _giLinkEntity         :: (Ref EdictT) -> Quake ()
              , _giUnlinkEntity       :: (Ref EdictT) -> Quake ()
              , _giBoxEdicts          :: V3 Float -> V3 Float -> Lens' QuakeState (V.Vector (Ref EdictT)) -> Int -> Int -> Quake Int
              , _giPMove              :: PMoveT -> Quake PMoveT
              , _giMulticast          :: V3 Float -> Int -> Quake ()
              , _giUnicast            :: (Ref EdictT) -> Bool -> Quake ()
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
  TraceT { _tAllSolid   :: Bool
         , _tStartSolid :: Bool
         , _tFraction   :: Float
         , _tEndPos     :: V3 Float
         , _tPlane      :: CPlaneT
         , _tSurface    :: Maybe CSurfaceT
         , _tContents   :: Int
         , _tEnt        :: Maybe (Ref EdictT)
         }

data PMoveT =
  PMoveT { _pmState         :: PMoveStateT
         , _pmCmd           :: UserCmdT
         , _pmSnapInitial   :: Bool
         , _pmNumTouch      :: Int
         , _pmTouchEnts     :: V.Vector (Ref EdictT)
         , _pmViewAngles    :: V3 Float
         , _pmViewHeight    :: Float
         , _pmMins          :: V3 Float
         , _pmMaxs          :: V3 Float
         , _pmGroundEntity  :: Maybe (Ref EdictT)
         , _pmWaterType     :: Int
         , _pmWaterLevel    :: Int
         , _pmTrace         :: V3 Float -> V3 Float -> V3 Float -> V3 Float -> Quake (Maybe TraceT)
         , _pmPointContents :: V3 Float -> Quake Int
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
                  , _gbGEdicts           :: MV.IOVector EdictT
                  , _gbItemList          :: V.Vector GItemT
                  , _gbPushed            :: V.Vector PushedT
                  , _gbPushedP           :: Int
                  , _gbObstacle          :: Maybe (Ref EdictT)
                  , _gbCYes              :: Int
                  , _gbCNo               :: Int
                  , _gbTouch             :: V.Vector (Ref EdictT)
                  , _gbIsQuad            :: Bool
                  , _gbIsSilenced        :: Int
                  , _gbCurrentPlayer     :: Maybe (Ref EdictT)
                  , _gbCurrentClient     :: Maybe (Ref GClientT)
                  , _gbForward           :: V3 Float
                  , _gbRight             :: V3 Float
                  , _gbUp                :: V3 Float
                  , _gbXYSpeed           :: Float
                  , _gbBobMove           :: Float
                  , _gbBobCycle          :: Int
                  , _gbBobFracSin        :: Float
                  , _gbXxxi              :: Int
                  , _gbEnemyVis          :: Bool
                  , _gbEnemyInFront      :: Bool
                  , _gbEnemyRange        :: Int
                  , _gbEnemyYaw          :: Float
                  , _gbPlayerDieIdx      :: Int
                  , _gbWindSound         :: Int
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
  MonsterInfoT { _miCurrentMove     :: Maybe MMoveT
               , _miAIFlags         :: Int
               , _miNextFrame       :: Int
               , _miScale           :: Float
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
               , _csCinematicFile          :: Maybe Handle
               , _csCinematicTime          :: Int
               , _csCinematicFrame         :: Int
               , _csCinematicPalette       :: B.ByteString
               , _csCinematicPaletteActive :: Bool
               , _csAttractLoop            :: Bool
               , _csServerCount            :: Int
               , _csGameDir                :: B.ByteString
               , _csPlayerNum              :: Int
               , _csConfigStrings          :: V.Vector B.ByteString
               , _csModelDraw              :: V.Vector (Maybe (IORef ModelT))
               , _csModelClip              :: V.Vector (Maybe (Ref CModelT))
               , _csSoundPrecache          :: V.Vector (Maybe (IORef SfxT))
               , _csImagePrecache          :: V.Vector (Maybe (IORef ImageT))
               , _csClientInfo             :: V.Vector ClientInfoT
               , _csBaseClientInfo         :: ClientInfoT
               }

data GLPolyT =
  GLPolyT { _glpNext           :: Maybe GLPolyReference
          , _glpChain          :: Maybe GLPolyReference
          , _glpNumVerts       :: Int
          , _glpFlags          :: Int
          , _glpPos            :: Int
          }

data MTexInfoT =
  MTexInfoT { _mtiVecs      :: (V4 Float, V4 Float)
            , _mtiFlags     :: Int
            , _mtiNumFrames :: Int
            , _mtiNext      :: Maybe (IORef MTexInfoT)
            , _mtiImage     :: Maybe (IORef ImageT)
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
         , _iTextureChain         :: Maybe (IORef MSurfaceT)
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
          , _rdEntities     :: V.Vector (IORef EntityT)
          , _rdNumDLights   :: Int
          , _rdDLights      :: V.Vector DLightT
          , _rdNumParticles :: Int
          }

data EntityT =
  EntityT { _eModel      :: Maybe (IORef ModelT)
          , _eAngles     :: V3 Float
          , _eOrigin     :: V3 Float
          , _eFrame      :: Int
          , _eOldOrigin  :: V3 Float
          , _eOldFrame   :: Int
          , _eBackLerp   :: Float
          , _eSkinNum    :: Int
          , _eLightStyle :: Int
          , _eAlpha      :: Float
          , _eSkin       :: Maybe (IORef ImageT)
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
         , _mSubModels            :: V.Vector (IORef MModelT)
         , _mNumPlanes            :: Int
         , _mPlanes               :: V.Vector (IORef CPlaneT)
         , _mNumLeafs             :: Int
         , _mLeafs                :: V.Vector (IORef MLeafT)
         , _mNumVertexes          :: Int
         , _mVertexes             :: V.Vector (IORef MVertexT)
         , _mNumEdges             :: Int
         , _mEdges                :: V.Vector (IORef MEdgeT)
         , _mNumNodes             :: Int
         , _mFirstNode            :: Int
         , _mNodes                :: V.Vector (IORef MNodeT)
         , _mNumTexInfo           :: Int
         , _mTexInfo              :: V.Vector (IORef MTexInfoT)
         , _mNumSurfaces          :: Int
         , _mSurfaces             :: V.Vector (IORef MSurfaceT)
         , _mNumSurfEdges         :: Int
         , _mSurfEdges            :: V.Vector Int
         , _mNumMarkSurfaces      :: Int
         , _mMarkSurfaces         :: V.Vector (IORef MSurfaceT)
         , _mVis                  :: Maybe DVisT
         , _mLightdata            :: Maybe B.ByteString
         , _mSkins                :: V.Vector (Maybe (IORef ImageT))
         , _mExtraDataSize        :: Int
         , _mExtraData            :: Maybe ModelExtra
         }

data MSurfaceT =
  MSurfaceT { _msVisFrame           :: Int
            , _msPlane              :: Maybe (IORef CPlaneT)
            , _msFlags              :: Int
            , _msFirstEdge          :: Int
            , _msNumEdges           :: Int
            , _msTextureMins        :: (Int16, Int16)
            , _msExtents            :: (Int16, Int16)
            , _msLightS             :: Int
            , _msLightT             :: Int
            , _msDLightS            :: Int
            , _msDLightT            :: Int
            , _msPolys              :: Maybe GLPolyReference
            , _msTextureChain       :: Maybe (IORef MSurfaceT)
            , _msLightmapChain      :: Maybe (IORef MSurfaceT)
            , _msTexInfo            :: MTexInfoT
            , _msDLightFrame        :: Int
            , _msDLightBits         :: Int
            , _msLightmapTextureNum :: Int
            , _msStyles             :: B.ByteString
            , _msCachedLight        :: UV.Vector Float
            , _msSamples            :: Maybe B.ByteString
            }

data MLeafT =
  MLeafT { _mlContents        :: Int
         , _mlVisFrame        :: Int
         , _mlMins            :: V3 Float
         , _mlMaxs            :: V3 Float
         , _mlParent          :: Maybe (IORef MNodeT)
         , _mlCluster         :: Int
         , _mlArea            :: Int
         , _mlNumMarkSurfaces :: Int
         , _mlMarkIndex       :: Int
         }

data ClientInfoT =
  ClientInfoT { _ciName        :: B.ByteString
              , _ciCInfo       :: B.ByteString
              , _ciSkin        :: Maybe (IORef ImageT)
              , _ciIcon        :: Maybe (IORef ImageT)
              , _ciIconName    :: B.ByteString
              , _ciModel       :: Maybe (IORef ModelT)
              , _ciWeaponModel :: V.Vector (Maybe (IORef ModelT))
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
             , _scrLastFrames      :: Int
             , _scrLastTime        :: Int
             , _scrFPSValue        :: B.ByteString
             , _scrCin             :: CinematicsT
             , _scrCenterString    :: B.ByteString
             , _scrCenterTimeStart :: Float
             , _scrCenterTimeOff   :: Float
             , _scrCenterLines     :: Int
             }

data RefExportT =
  RefExportT { _reInit                :: Int -> Int -> Quake Bool
             , _reShutDown            :: Quake ()
             , _reBeginRegistration   :: B.ByteString -> Quake ()
             , _reRegisterModel       :: B.ByteString -> Quake (Maybe (IORef ModelT))
             , _reRegisterSkin        :: B.ByteString -> Quake (Maybe (IORef ImageT))
             , _reRegisterPic         :: B.ByteString -> Quake (Maybe (IORef ImageT))
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
    entInteract :: a -> (Ref EdictT) -> (Ref EdictT) -> Quake Bool

class EntThinkAdapter a where
    think :: a -> (Ref EdictT) -> Quake Bool

class EntBlockedAdapter a where
    blocked :: a -> (Ref EdictT) -> (Ref EdictT) -> Quake ()

class EntDodgeAdapter a where
    dodge :: a -> (Ref EdictT) -> (Ref EdictT) -> Float -> Quake ()

class EntTouchAdapter a where
    touch :: a -> (Ref EdictT) -> (Ref EdictT) -> CPlaneT -> Maybe CSurfaceT -> Quake ()

class EntUseAdapter a where
    entUse :: a -> (Ref EdictT) -> Maybe (Ref EdictT) -> Maybe (Ref EdictT) -> Quake ()

class EntPainAdapter a where
    pain :: a -> (Ref EdictT) -> (Ref EdictT) -> Float -> Int -> Quake ()

class EntDieAdapter a where
    die :: a -> (Ref EdictT) -> (Ref EdictT) -> (Ref EdictT) -> Int -> V3 Float -> Quake ()

class ItemUseAdapter a where
    itemUse :: a -> (Ref EdictT) -> GItemReference -> Quake ()

class ItemDropAdapter a where
    itemDrop :: a -> (Ref EdictT) -> GItemReference -> Quake ()

class AIAdapter a where
    ai :: a -> (Ref EdictT) -> Float -> Quake ()

data EntInteract =
    PickupArmor B.ByteString ((Ref EdictT) -> (Ref EdictT) -> Quake Bool)
  | PickupPowerArmor B.ByteString ((Ref EdictT) -> (Ref EdictT) -> Quake Bool)
  | PickupHealth B.ByteString ((Ref EdictT) -> (Ref EdictT) -> Quake Bool)
  | PickupAdrenaline B.ByteString ((Ref EdictT) -> (Ref EdictT) -> Quake Bool)
  | PickupAncientHead B.ByteString ((Ref EdictT) -> (Ref EdictT) -> Quake Bool)
  | GenericEntInteract { _geiId :: B.ByteString
                       , _geiInteract :: (Ref EdictT) -> (Ref EdictT) -> Quake Bool
                       }

data EntThink =
  GenericEntThink { _gethId :: B.ByteString
                  , _gethThink :: (Ref EdictT) -> Quake Bool
                  }

data EntBlocked =
  GenericEntBlocked { _gebId :: B.ByteString
                    , _gebBlocked :: (Ref EdictT) -> (Ref EdictT) -> Quake ()
                    }

data EntDodge =
  GenericEntDodge { _gedoId :: B.ByteString
                  , _gedoDodge :: (Ref EdictT) -> (Ref EdictT) -> Float -> Quake ()
                  }

data EntTouch =
  GenericEntTouch { _getId :: B.ByteString
                  , _getTouch :: (Ref EdictT) -> (Ref EdictT) -> CPlaneT -> Maybe CSurfaceT -> Quake ()
                  }

data EntUse =
    FuncExplosiveUse B.ByteString ((Ref EdictT) -> Maybe (Ref EdictT) -> Maybe (Ref EdictT) -> Quake ())
  | GenericEntUse { _geuId :: B.ByteString
                , _geuUse :: (Ref EdictT) -> Maybe (Ref EdictT) -> Maybe (Ref EdictT) -> Quake ()
                }

data EntPain =
  GenericEntPain { _gepId :: B.ByteString
                 , _gepPain :: (Ref EdictT) -> (Ref EdictT) -> Float -> Int -> Quake ()
                 }

data EntDie =
  GenericEntDie { _gedId :: B.ByteString
                , _gedDie :: (Ref EdictT) -> (Ref EdictT) -> (Ref EdictT) -> Int -> V3 Float -> Quake ()
                }

data ItemUse =
  GenericItemUse { _giuId :: B.ByteString
                 , _giuUse :: (Ref EdictT) -> GItemReference -> Quake ()
                 }

data ItemDrop =
  GenericItemDrop { _gidId :: B.ByteString
                  , _gidDrop :: (Ref EdictT) -> GItemReference -> Quake ()
                  }

data AI =
  GenericAI { _gaiId :: B.ByteString
            , _gaiAi :: (Ref EdictT) -> Float -> Quake ()
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
    itemDrop (GenericItemDrop _ _itemDrop) = _itemDrop

instance AIAdapter AI where
    ai (GenericAI _ _ai) = _ai

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
                    , _cpWeapon          :: Maybe GItemReference
                    , _cpLastWeapon      :: Maybe GItemReference
                    , _cpPowerCubes      :: Int
                    , _cpScore           :: Int
                    , _cpGameHelpChanged :: Int
                    , _cpHelpChanged     :: Int
                    , _cpSpectator       :: Bool
                    }

data CMGlobals =
  CMGlobals { _cmCheckCount      :: Int
            , _cmMapName         :: B.ByteString
            , _cmNumBrushSides   :: Int
            , _cmMapBrushSides   :: V.Vector CBrushSideT
            , _cmNumTexInfo      :: Int
            , _cmMapSurfaces     :: V.Vector MapSurfaceT
            , _cmNumPlanes       :: Int
            , _cmMapPlanes       :: MV.IOVector CPlaneT
            , _cmNumNodes        :: Int
            , _cmMapNodes        :: V.Vector CNodeT
            , _cmNumLeafs        :: Int
            , _cmMapLeafs        :: V.Vector CLeafT
            , _cmEmptyLeaf       :: Int
            , _cmSolidLeaf       :: Int
            , _cmNumLeafBrushes  :: Int
            , _cmMapLeafBrushes  :: UV.Vector Word16
            , _cmNumCModels      :: Int
            , _cmMapCModels      :: V.Vector CModelT
            , _cmNumBrushes      :: Int
            , _cmMapBrushes      :: MV.IOVector CBrushT
            , _cmNumVisibility   :: Int
            , _cmMapVisibility   :: B.ByteString
            , _cmMapVis          :: DVisT
            , _cmNumEntityChars  :: Int
            , _cmMapEntityString :: B.ByteString
            , _cmNumAreas        :: Int
            , _cmMapAreas        :: V.Vector CAreaT
            , _cmNumAreaPortals  :: Int
            , _cmMapAreaPortals  :: V.Vector DAreaPortalT
            , _cmNumClusters     :: Int
            , _cmFloodValid      :: Int
            , _cmPortalOpen      :: UV.Vector Bool
            , _cmCModBase        :: Maybe BL.ByteString
            , _cmChecksum        :: Int
            , _cmLastChecksum    :: Int
            , _cmDebugLoadMap    :: Bool
            , _cmBoxHeadNode     :: Int
            , _cmLeafCount       :: Int
            , _cmLeafMaxCount    :: Int
            , _cmLeafMins        :: V3 Float
            , _cmLeafMaxs        :: V3 Float
            , _cmLeafTopNode     :: Int
            , _cmTraceStart      :: V3 Float
            , _cmTraceEnd        :: V3 Float
            , _cmTraceMins       :: V3 Float
            , _cmTraceMaxs       :: V3 Float
            , _cmTraceExtents    :: V3 Float
            , _cmTraceTrace      :: TraceT
            , _cmTraceContents   :: Int
            , _cmTraceIsPoint    :: Bool
            , _cmLeafs           :: UV.Vector Int -- tmp for CM.boxTrace
            }

data MoveInfoT =
  MoveInfoT { _miStartOrigin       :: V3 Float
            , _miStartAngles       :: V3 Float
            , _miEndOrigin         :: V3 Float
            , _miEndAngles         :: V3 Float
            , _miSoundStart        :: Int
            , _miSoundMiddle       :: Int
            , _miSoundEnd          :: Int
            , _miAccel             :: Float
            , _miSpeed             :: Float
            , _miDecel             :: Float
            , _miDistance          :: Float
            , _miWait              :: Float
            , _miState             :: Int
            , _miDir               :: V3 Float
            , _miCurrentSpeed      :: Float
            , _miMoveSpeed         :: Float
            , _miNextSpeed         :: Float
            , _miRemainingDistance :: Float
            , _miDecelDistance     :: Float
            , _miEndFunc           :: Maybe EntThink
            }

data CEntityT =
  CEntityT { _ceBaseline    :: EntityStateT
           , _ceCurrent     :: EntityStateT
           , _cePrev        :: EntityStateT
           , _ceServerFrame :: Int
           , _ceTrailCount  :: Int
           , _ceLerpOrigin  :: V3 Float
           , _ceFlyStopTime :: Int
           }

data AreaNodeT =
  AreaNodeT { _anAxis          :: Int
            , _anDist          :: Float
            , _anChildren      :: (Maybe Int, Maybe Int) -- indexes to svGlobals.svAreaNodes
            , _anTriggerEdicts :: Ref LinkT
            , _anSolidEdicts   :: Ref LinkT
            }

data LinkT =
  LinkT { _lIndex :: Int
        , _lPrev  :: Maybe (Ref LinkT)
        , _lNext  :: Maybe (Ref LinkT)
        , _lEdict :: Maybe (Ref EdictT)
        }

data SpawnT =
  SpawnT { _spName  :: B.ByteString
         , _spSpawn :: EntThink
         }

data GameItemsGlobals =
  GameItemsGlobals { _giJacketArmorInfo      :: GItemArmorT
                   , _giCombatArmorInfo      :: GItemArmorT
                   , _giBodyArmorInfo        :: GItemArmorT
                   , _giQuakeDropTimeoutHack :: Int
                   , _giJacketArmorIndex     :: GItemReference
                   , _giCombatArmorIndex     :: GItemReference
                   , _giBodyArmorIndex       :: GItemReference
                   , _giPowerScreenIndex     :: GItemReference
                   , _giPowerShieldIndex     :: GItemReference
                   }

data MSoldierGlobals =
  MSoldierGlobals { _msSoundIdle       :: Int
                  , _msSoundSight1     :: Int
                  , _msSoundSight2     :: Int
                  , _msSoundPainLight  :: Int
                  , _msSoundPain       :: Int
                  , _msSoundPainSS     :: Int
                  , _msSoundDeathLight :: Int
                  , _msSoundDeath      :: Int
                  , _msSoundDeathSS    :: Int
                  , _msSoundCock       :: Int
                  }

data MInfantryGlobals =
  MInfantryGlobals { _miSoundPain1      :: Int
                   , _miSoundPain2      :: Int
                   , _miSoundDie1       :: Int
                   , _miSoundDie2       :: Int
                   , _miSoundGunShot    :: Int
                   , _miSoundWeaponCock :: Int
                   , _miSoundPunchSwing :: Int
                   , _miSoundPunchHit   :: Int
                   , _miSoundSight      :: Int
                   , _miSoundSearch     :: Int
                   , _miSoundIdle       :: Int
                   }

data MFrameT =
  MFrameT { _mfAI    :: Maybe AI
          , _mfDist  :: Float
          , _mfThink :: Maybe EntThink
          }

data MMoveT =
  MMoveT { _mmId         :: B.ByteString -- to check for equality
         , _mmFirstFrame :: Int
         , _mmLastFrame  :: Int
         , _mmFrame      :: V.Vector MFrameT
         , _mmEndFunc    :: Maybe EntThink
         }

data PlayerTrailGlobals =
  PlayerTrailGlobals { _ptTrail       :: V.Vector (Ref EdictT)
                     , _ptTrailHead   :: Int
                     , _ptTrailActive :: Bool
                     }

data PushedT =
  PushedT { _pEnt      :: Maybe (Ref EdictT)
          , _pOrigin   :: V3 Float
          , _pAngles   :: V3 Float
          , _pDeltaYaw :: Float
          }

data VIDGlobals =
  VIDGlobals { _vgVidModes           :: V.Vector VidModeT
             , _vgRefLibActive       :: Bool
             , _vgFSModes            :: Maybe (V.Vector VidModeT)
             , _vgFSResolutions      :: V.Vector B.ByteString
             , _vgModeX              :: Int
             , _vgRefs               :: V.Vector B.ByteString
             , _vgDrivers            :: V.Vector B.ByteString
             , _vgCurrentMenu        :: Maybe (Ref MenuFrameworkS)
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
            , _rRegisterModel     :: GLDriver -> B.ByteString -> Quake (Maybe (IORef ModelT))
            , _rRegisterSkin      :: GLDriver -> B.ByteString -> Quake (Maybe (IORef ImageT))
            , _rDrawFindPic       :: GLDriver -> B.ByteString -> Quake (Maybe (IORef ImageT))
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
            , _glScreenShotF      :: GLDriver -> Quake ()
            }

data INGlobals =
  INGlobals { _inMouseAvail          :: Bool
            , _inMouseActive         :: Bool
            , _inIgnoreFirst         :: Bool
            , _inMouseButtonState    :: Int
            , _inMouseOldButtonState :: Int
            , _inOldMouseX           :: Int
            , _inOldMouseY           :: Int
            , _inMLooking            :: Bool
            }

data GLFWKBDEvent =
    KeyPress GLFW.Key
  | KeyRelease GLFW.Key
  | CursorPosition Double Double
  | MouseButtonPress GLFW.MouseButton
  | MouseButtonRelease GLFW.MouseButton
  | MouseWheelScroll Double
  | ConfigureNotify

data GLFWbGlobals =
  GLFWbGlobals { _glfwbOldDisplayMode :: Maybe GLFW.VideoMode
               , _glfwbWindow         :: Maybe GLFW.Window
               , _glfwbWindowXPos     :: Int
               , _glfwbWindowYPos     :: Int
               , _glfwbKBDChan        :: Maybe (TChan GLFWKBDEvent)
               }

data KBDGlobals =
  KBDGlobals { _kbdMx    :: Int
             , _kbdMy    :: Int
             , _kbdWinx  :: Int
             , _kbdWiny  :: Int
             , _kbdWinW2 :: Int
             , _kbdWinH2 :: Int
             , _kbdX     :: Int
             , _kbdY     :: Int
             }

data BasicRenderAPIGlobals =
  BasicRenderAPIGlobals

data FastRenderAPIGlobals =
  FastRenderAPIGlobals { _frGLDepthMin           :: Float
                       , _frGLDepthMax           :: Float
                       , _frGLConfig             :: GLConfigT
                       , _frGLState              :: GLStateT
                       , _frd8to24table          :: UV.Vector Int
                       , _frVid                  :: VidDefT
                       , _frColorTableEXT        :: Bool
                       , _frActiveTextureARB     :: Bool
                       , _frPointParameterEXT    :: Bool
                       , _frLockArraysEXT        :: Bool
                       , _frSwapIntervalEXT      :: Bool
                       , _frTexture0             :: Int
                       , _frTexture1             :: Int
                       , _frGLTexSolidFormat     :: Int
                       , _frGLTexAlphaFormat     :: Int
                       , _frGLFilterMin          :: Int
                       , _frGLFilterMax          :: Int
                       , _frNumGLTextures        :: Int
                       , _frGLTextures           :: V.Vector (IORef ImageT)
                       , _frLastModes            :: (Int, Int)
                       , _frRegistrationSequence :: Int
                       , _frGammaTable           :: B.ByteString
                       , _frIntensityTable       :: B.ByteString
                       , _frModKnown             :: V.Vector (IORef ModelT)
                       , _frModNumKnown          :: Int
                       , _frLoadModel            :: IORef ModelT
                       , _frCurrentModel         :: Maybe (IORef ModelT)
                       , _frModInline            :: V.Vector (IORef ModelT)
                       , _frModNoVis             :: B.ByteString
                       , _frNoTexture            :: IORef ImageT
                       , _frParticleTexture      :: IORef ImageT
                       , _frUploadWidth          :: Int
                       , _frUploadHeight         :: Int
                       , _frUploadedPaletted     :: Bool
                       , _frDrawChars            :: Maybe (IORef ImageT)
                       , _frTrickFrame           :: Int
                       , _frScrapDirty           :: Bool
                       , _frViewCluster          :: Int
                       , _frViewCluster2         :: Int
                       , _frOldViewCluster       :: Int
                       , _frOldViewCluster2      :: Int
                       , _frWorldModel           :: Maybe (IORef ModelT)
                       , _frModelTextureCoordBuf :: MSV.IOVector Float
                       , _frModelVertexIndexBuf  :: MSV.IOVector Int32
                       , _frModelTextureCoordIdx :: Int
                       , _frModelVertexIndexIdx  :: Int
                       , _frPolygonS1Old         :: UV.Vector Float
                       , _frPolygonBuffer        :: MSV.IOVector Float
                       , _frPolygonCache         :: MV.IOVector GLPolyT
                       , _frPolygonBufferIndex   :: Int
                       , _frPolygonCount         :: Int
                       , _frGLLms                :: GLLightMapStateT
                       , _frNewRefDef            :: RefDefT
                       , _frFrameCount           :: Int
                       , _frWarpFace             :: Maybe MSurfaceT
                       , _frModelVisibility      :: Maybe B.ByteString
                       , _frSkyName              :: B.ByteString
                       , _frSkyRotate            :: Float
                       , _frSkyAxis              :: V3 Float
                       , _frSkyImages            :: V.Vector (Maybe (IORef ImageT))
                       , _frSkyMin               :: Float
                       , _frSkyMax               :: Float
                       , _frCBrushPolys          :: Int
                       , _frCAliasPolys          :: Int
                       , _frFrustum              :: V.Vector (IORef CPlaneT)
                       , _frDLightFrameCount     :: Int
                       , _frOrigin               :: V3 Float
                       , _frVUp                  :: V3 Float
                       , _frVPn                  :: V3 Float
                       , _frVRight               :: V3 Float
                       , _frVBlend               :: V4 Float
                       , _frWorldMatrix          :: [GL.GLfloat]
                       , _frVisFrameCount        :: Int
                       , _frModelOrg             :: V3 Float
                       , _frCurrentEntity        :: Maybe (IORef EntityT)
                       , _frSkyMins              :: (UV.Vector Float, UV.Vector Float)
                       , _frSkyMaxs              :: (UV.Vector Float, UV.Vector Float)
                       , _frAlphaSurfaces        :: Maybe (IORef MSurfaceT)
                       , _frBlockLights          :: UV.Vector Float
                       , _frPointColor           :: V3 Float
                       , _frLightSpot            :: V3 Float
                       , _frVertexArrayBuf       :: MSV.IOVector Float
                       , _frRawPalette           :: UV.Vector Int
                       }

data ParticleTGlobals =
  ParticleTGlobals { _pColorTable  :: UV.Vector Int32
                   , _pVertexArray :: MSV.IOVector Float
                   , _pColorArray  :: MSV.IOVector Int32
                   }

data MenuFrameworkS =
  MenuFrameworkS { _mfX          :: Int
                 , _mfY          :: Int
                 , _mfCursor     :: Int
                 , _mfNItems     :: Int
                 , _mfNSlots     :: Int
                 , _mfItems      :: V.Vector MenuItemRef
                 , _mfStatusBar  :: Maybe B.ByteString
                 , _mfCursorDraw :: Maybe (Quake ())
                 }
                 
data MenuCommonS =
  MenuCommonS { _mcType          :: Int
              , _mcName          :: Maybe B.ByteString
              , _mcX             :: Int
              , _mcY             :: Int
              , _mcParent        :: Maybe (Ref MenuFrameworkS)
              , _mcCursorOffset  :: Int
              , _mcLocalData     :: V4 Int
              , _mcFlags         :: Int
              , _mcN             :: Int
              , _mcStatusBar     :: Maybe B.ByteString
              , _mcCallback      :: Maybe (Quake ())
              , _mcStatusBarFunc :: Maybe (Quake ())
              , _mcOwnerDraw     :: Maybe (Quake ())
              , _mcCursorDraw    :: Maybe (Quake ())
              }

data MenuListS =
  MenuListS { _mlGeneric   :: MenuCommonS
            , _mlCurValue  :: Int
            , _mlItemNames :: V.Vector B.ByteString
            }

data MenuSliderS =
  MenuSliderS { _msGeneric  :: MenuCommonS
              , _msMinValue :: Float
              , _msMaxValue :: Float
              , _msCurValue :: Float
              , _msRange    :: Float
              }

data MenuActionS =
  MenuActionS { _maGeneric :: MenuCommonS
              }

data MenuSeparatorS =
  MenuSeparatorS { _mspGeneric :: MenuCommonS
                 }

data MenuFieldS =
  MenuFieldS { _mflGeneric       :: MenuCommonS
             , _mflBuffer        :: B.ByteString
             , _mflCursor        :: Int
             , _mflLength        :: Int
             , _mflVisibleLength :: Int
             , _mflVisibleOffset :: Int
             }

data MenuGlobals =
  MenuGlobals { _mgMenuFrameworks      :: V.Vector MenuFrameworkS
              , _mgMenuListSItems      :: V.Vector MenuListS
              , _mgMenuSliderSItems    :: V.Vector MenuSliderS
              , _mgMenuActionSItems    :: V.Vector MenuActionS
              , _mgMenuSeparatorSItems :: V.Vector MenuSeparatorS
              , _mgMenuFieldSItems     :: V.Vector MenuFieldS
              , _mgLayers              :: V.Vector MenuLayerT
              , _mgDrawFunc            :: Maybe XCommandT
              , _mgKeyFunc             :: Maybe KeyFuncT
              , _mgEnterSound          :: Bool
              , _mgMenuDepth           :: Int
              , _mgMainCursor          :: Int
              , _mgCached              :: Bool
              , _mgGameCursor          :: Int
              , _mgSaveStrings         :: V.Vector B.ByteString
              , _mgSaveValid           :: V.Vector Bool
              , _mgLocalServerNames    :: V.Vector B.ByteString
              , _mgLocalServerNetAdr   :: V.Vector NetAdrT
              , _mgCreditsStartTime    :: Int
              , _mgCredits             :: V.Vector B.ByteString
              , _mgNumServers          :: Int
              , _mgBindGrab            :: Bool
              , _mgDmOptionsStatusBar  :: Maybe B.ByteString
              , _mgMapNames            :: Maybe (V.Vector B.ByteString)
              }

data MenuLayerT =
  MenuLayerT { _mlDraw :: Maybe XCommandT
             , _mlKey  :: Maybe KeyFuncT
             }

data ClientGlobals =
  ClientGlobals { _cgExtraTime          :: Int
                , _cgNumCheatVars       :: Int
                , _cgBuf                :: SizeBufT
                , _cgFrameMsec          :: Int64
                , _cgOldSysFrameTime    :: Int64
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
                , _cgInImpulse          :: Int
                , _cgDLights            :: V.Vector (IORef CDLightT)
                , _cgLightStyle         :: V.Vector CLightStyleT
                , _cgLastOfs            :: Int
                , _cgCR                 :: Int -- from Console.hs
                , _cgParticles          :: V.Vector (IORef CParticleT)
                , _cgActiveParticles    :: Maybe (IORef CParticleT)
                , _cgFreeParticles      :: Maybe (IORef CParticleT)
                , _cgPrecacheCheck      :: Int
                , _cgPrecacheSpawnCount :: Int
                , _cgPrecacheTex        :: Int
                , _cgPrecacheModelSkin  :: Int
                , _cgPrecacheModel      :: Maybe B.ByteString
                , _cgNumCLWeaponModels  :: Int
                , _cgWeaponModels       :: V.Vector B.ByteString
                , _cgPMPassEnt          :: Maybe (Ref EdictT)
                , _cgIsDown             :: Bool
                , _cgAVelocities        :: V.Vector (V3 Float)
                }

data VRectT =
  VRectT { _vrX      :: Int
         , _vrY      :: Int
         , _vrWidth  :: Int
         , _vrHeight :: Int
         }

data VGlobals =
  VGlobals { _vgNumDLights   :: Int
           , _vgNumEntities  :: Int
           , _vgNumParticles :: Int
           , _vgLightStyles  :: V.Vector LightStyleT
           , _vgEntities     :: V.Vector (IORef EntityT)
           , _vgDLights      :: V.Vector DLightT
           }

data CDLightT =
  CDLightT { _cdlKey      :: Int
           , _cdlColor    :: V3 Float
           , _cdlOrigin   :: V3 Float
           , _cdlRadius   :: Float
           , _cdlDie      :: Float
           , _cdlMinLight :: Float
           }

data CLightStyleT =
  CLightStyleT { _clsLength :: Int
               , _clsValue  :: V3 Float
               , _clsMap    :: UV.Vector Float
               }

data CinematicsT =
  CinematicsT { _cRestartSound :: Bool
              , _cSRate        :: Int
              , _cSWidth       :: Int
              , _cSChannels    :: Int
              , _cWidth        :: Int
              , _cHeight       :: Int
              , _cPic          :: Maybe B.ByteString
              , _cPicPending   :: Maybe B.ByteString
              , _cHNodes1      :: Maybe (MV.IOVector Int)
              , _cNumHNodes1   :: UV.Vector Int
              , _cHUsed        :: UV.Vector Int
              , _cHCount       :: UV.Vector Int
              }

data NetChannelGlobals =
  NetChannelGlobals { _ncSendBuf :: B.ByteString
                    , _ncSend    :: SizeBufT
                    }

data CParticleT =
  CParticleT { _cpTime     :: Float
             , _cpOrg      :: V3 Float
             , _cpVel      :: V3 Float
             , _cpAccel    :: V3 Float
             , _cpColor    :: Float
             , _cpAlpha    :: Float
             , _cpAlphaVel :: Float
             , _cpNext     :: Maybe (IORef CParticleT)
             }

data GLLightMapStateT =
  GLLightMapStateT { _lmsInternalFormat         :: Int
                   , _lmsCurrentLightmapTexture :: Int
                   , _lmsLightmapSurfaces       :: V.Vector (IORef MSurfaceT)
                   , _lmsAllocated              :: UV.Vector Int
                   , _lmsLightmapBuffer         :: MSV.IOVector Word8
                   }

data MNodeT =
  MNodeT { _mnContents     :: Int
         , _mnVisFrame     :: Int
         , _mnMins         :: V3 Float
         , _mnMaxs         :: V3 Float
         , _mnParent       :: Maybe (IORef MNodeT)
         , _mnPlane        :: IORef CPlaneT
         , _mnChildren     :: (MNodeChild, MNodeChild)
         , _mnFirstSurface :: Int
         , _mnNumSurfaces  :: Int
         }

data BeamT =
  BeamT { _bEntity     :: Int
        , _bDestEntity :: Int
        , _bModel      :: Maybe (IORef ModelT)
        , _bEndTime    :: Int
        , _bOffset     :: V3 Float
        , _bStart      :: V3 Float
        , _bEnd        :: V3 Float
        }

data CLSustainT =
  CLSustainT { _clsId            :: Int
             , _clsType          :: Int
             , _clsEndTime       :: Int
             , _clsNextThink     :: Int
             , _clsThinkInterval :: Int
             , _clsOrg           :: V3 Float
             , _clsDir           :: V3 Float
             , _clsColor         :: Int
             , _clsCount         :: Int
             , _clsMagnitude     :: Int
             , _clsThink         :: Maybe (CLSustainT -> Quake Bool)
             }

data LaserT =
  LaserT { _lEnt     :: IORef EntityT
         , _lEndTime :: Int
         }

data ExplosionT =
  ExplosionT { _eType       :: Int
             , _eEnt        :: IORef EntityT
             , _eFrames     :: Int
             , _eLight      :: Float
             , _eLightColor :: V3 Float
             , _eStart      :: Float
             , _eBaseFrame  :: Int
             }

data ClientStaticT = ClientStaticT
    { _csState              :: Int
    , _csKeyDest            :: Int
    , _csFrameCount         :: Int
    , _csRealTime           :: Int
    , _csFrameTime          :: Float
    , _csDisableScreen      :: Float
    , _csDisableServerCount :: Int
    , _csServerName         :: B.ByteString
    , _csConnectTime        :: Float
    , _csQuakePort          :: Int
    , _csNetChan            :: NetChanT
    , _csServerProtocol     :: Int
    , _csChallenge          :: Int
    , _csDownload           :: Maybe Handle
    , _csDownloadTempName   :: B.ByteString
    , _csDownloadName       :: B.ByteString
    , _csDownloadNumber     :: Int
    , _csDownloadType       :: Int
    , _csDownloadPercent    :: Int
    , _csDemoRecording      :: Bool
    , _csDemoWaiting        :: Bool
    , _csDemoFile           :: Maybe Handle
    }

data ConsoleT = ConsoleT
    { _cInitialized :: Bool
    , _cText        :: MSV.IOVector Char
    , _cCurrent     :: Int
    , _cX           :: Int
    , _cDisplay     :: Int
    , _cOrMask      :: Int
    , _cLineWidth   :: Int
    , _cTotalLines  :: Int
    , _cCursorSpeed :: Float
    , _cVisLines    :: Int
    , _cTimes       :: UV.Vector Float
    }

data DirtyT = DirtyT
    { _x1 :: Int
    , _x2 :: Int
    , _y1 :: Int
    , _y2 :: Int
    }

data DLightT = DLightT
    { _dlOrigin    :: V3 Float
    , _dlColor     :: V3 Float
    , _dlIntensity :: Float
    }

data FrameT = FrameT
    { _fValid         :: Bool
    , _fServerFrame   :: Int
    , _fServerTime    :: Int
    , _fDeltaFrame    :: Int
    , _fAreaBits      :: UV.Vector Word8
    , _fPlayerState   :: PlayerStateT
    , _fNumEntities   :: Int
    , _fParseEntities :: Int
    }

data KButtonT = KButtonT
    { _kbDown     :: (Int, Int)
    , _kbDownTime :: Int64
    , _kbMsec     :: Int64
    , _kbState    :: Int
    }

data LightStyleT = LightStyleT
    { _lsRGB   :: V3 Float
    , _lsWhite :: Float
    }

data VidDefT = VidDefT
    { _vdWidth     :: Int
    , _vdHeight    :: Int
    , _vdNewWidth  :: Int
    , _vdNewHeight :: Int
    }

data VidModeT = VidModeT
    { _vmDescription :: B.ByteString
    , _vmWidth       :: Int
    , _vmHeight      :: Int
    , _vmMode        :: Int
    }

data CmdAliasT = CmdAliasT
    { _caName  :: B.ByteString
    , _caValue :: B.ByteString
    } deriving (Eq)

data CModelT = CModelT
    { _cmMins     :: V3 Float
    , _cmMaxs     :: V3 Float
    , _cmOrigin   :: V3 Float
    , _cmHeadNode :: Int
    }

data CPlaneT = CPlaneT
    { _cpNormal   :: V3 Float
    , _cpDist     :: Float
    , _cpType     :: Int8
    , _cpSignBits :: Int8
    , _cpPad      :: (Int8, Int8)
    }

data CSurfaceT = CSurfaceT
    { _csName  :: B.ByteString
    , _csFlags :: Int
    , _csValue :: Int
    }

data PmlT = PmlT
    { _pmlOrigin         :: V3 Float
    , _pmlVelocity       :: V3 Float
    , _pmlForward        :: V3 Float
    , _pmlRight          :: V3 Float
    , _pmlUp             :: V3 Float
    , _pmlFrameTime      :: Float
    , _pmlGroundSurface  :: Maybe CSurfaceT
    , _pmlGroundContents :: Int
    , _pmlPreviousOrigin :: V3 Float
    , _pmlLadder         :: Bool
    }

data MapSurfaceT = MapSurfaceT
    { _msCSurface :: CSurfaceT
    , _msRName    :: Maybe B.ByteString
    }

data CVarT = CVarT
    { _cvName          :: B.ByteString
    , _cvString        :: B.ByteString
    , _cvLatchedString :: Maybe B.ByteString
    , _cvFlags         :: Int
    , _cvModified      :: Bool
    , _cvValue         :: Float
    } deriving (Eq)

data GItemArmorT = GItemArmorT
    { _giaBaseCount        :: Int
    , _giaMaxCount         :: Int
    , _giaNormalProtection :: Float
    , _giaEnergyProtection :: Float
    , _giaArmor            :: Int
    }

data PMoveStateT = PMoveStateT
    { _pmsPMType      :: Int
    , _pmsOrigin      :: V3 Int16
    , _pmsVelocity    :: V3 Int16
    , _pmsPMFlags     :: Int8
    , _pmsPMTime      :: Int8
    , _pmsGravity     :: Int16
    , _pmsDeltaAngles :: V3 Int16
    } deriving Eq

data PlayerStateT = PlayerStateT
    { _psPMoveState :: PMoveStateT
    , _psViewAngles :: V3 Float
    , _psViewOffset :: V3 Float
    , _psKickAngles :: V3 Float
    , _psGunAngles  :: V3 Float
    , _psGunOffset  :: V3 Float
    , _psGunIndex   :: Int
    , _psGunFrame   :: Int
    , _psBlend      :: V4 Float
    , _psFOV        :: Float
    , _psRDFlags    :: Int
    , _psStats      :: UV.Vector Int16
    }

data ClientFrameT = ClientFrameT
    { _cfAreaBytes   :: Int
    , _cfAreaBits    :: SV.Vector Word8
    , _cfPlayerState :: PlayerStateT
    , _cfNumEntities :: Int
    , _cfFirstEntity :: Int
    , _cfSentTime    :: Int
    }

data SpawnTempT = SpawnTempT
    { _stSky       :: B.ByteString
    , _stSkyRotate :: Float
    , _stSkyAxis   :: V3 Float
    , _stNextMap   :: B.ByteString
    , _stLip       :: Int
    , _stDistance  :: Int
    , _stHeight    :: Int
    , _stNoise     :: Maybe B.ByteString
    , _stPauseTime :: Float
    , _stItem      :: Maybe B.ByteString
    , _stGravity   :: Maybe B.ByteString
    , _stMinYaw    :: Float
    , _stMaxYaw    :: Float
    , _stMinPitch  :: Float
    , _stMaxPitch  :: Float
    }

data UserCmdT = UserCmdT
    { _ucMsec        :: Int8
    , _ucButtons     :: Int8
    , _ucAngles      :: V3 Int16
    , _ucForwardMove :: Int16
    , _ucSideMove    :: Int16
    , _ucUpMove      :: Int16
    , _ucImpulse     :: Int8
    , _ucLightLevel  :: Int8
    }

data CAreaT = CAreaT
    { _caNumAreaPortals  :: Int
    , _caFirstAreaPortal :: Int
    , _caFloodNum        :: Int
    , _caFloodValid      :: Int
    }

data CBrushSideT = CBrushSideT
    { _cbsPlane   :: Maybe (Ref CPlaneT)
    , _cbsSurface :: Maybe (Ref MapSurfaceT) -- Nothing means nullsurface (from jake2)
    }

data CBrushT = CBrushT
    { _cbContents       :: Int
    , _cbNumSides       :: Int
    , _cbFirstBrushSide :: Int
    , _cbCheckCount     :: Int
    }

data CLeafT = CLeafT
    { _clContents       :: Int
    , _clCluster        :: Int
    , _clArea           :: Int
    , _clFirstLeafBrush :: Word16
    , _clNumLeafBrushes :: Word16
    }

data CNodeT = CNodeT
    { _cnPlane    :: Maybe (Ref CPlaneT)
    , _cnChildren :: (Int, Int)
    }

data FileLinkT = FileLinkT
    { _flFrom       :: B.ByteString
    , _flFromLength :: Int
    , _flTo         :: B.ByteString
    }

data NetAdrT = NetAdrT
    { _naType :: Int
    , _naPort :: Int
    , _naIP   :: Maybe HostAddress
    }

data NetChanT = NetChanT
    { _ncFatalError                   :: Bool
    , _ncSock                         :: Int -- TODO: is it Int or Socket??
    , _ncDropped                      :: Int
    , _ncLastReceived                 :: Int
    , _ncLastSent                     :: Int
    , _ncRemoteAddress                :: NetAdrT
    , _ncRemoteQPort                  :: Int
    , _ncIncomingSequence             :: Int
    , _ncIncomingAcknowledged         :: Int
    , _ncIncomingReliableAcknowledged :: Int
    , _ncIncomingReliableSequence     :: Int
    , _ncOutgoingSequence             :: Int
    , _ncReliableSequence             :: Int
    , _ncLastReliableSequence         :: Int
    , _ncMessage                      :: SizeBufT
    , _ncMessageBuf                   :: B.ByteString
    , _ncReliableLength               :: Int
    , _ncReliableBuf                  :: B.ByteString
    }

data ChallengeT = ChallengeT
    { _chAdr       :: NetAdrT
    , _chChallenge :: Int
    , _chTime      :: Int
    }

data SearchPathT = SearchPathT
    { _spFilename :: B.ByteString
    , _spPack     :: Maybe PackT
    } deriving (Eq)

data SizeBufT = SizeBufT
    { _sbAllowOverflow :: Bool
    , _sbOverflowed    :: Bool
    , _sbData          :: B.ByteString
    , _sbMaxSize       :: Int
    , _sbCurSize       :: Int
    , _sbReadCount     :: Int
    }

data DAreaPortalT = DAreaPortalT
    { _dapPortalNum :: Int
    , _dapOtherArea :: Int
    }

data PackT = PackT
    { _pFilename   :: B.ByteString
    , _pHandle     :: Maybe Handle
    , _pBackBuffer :: B.ByteString
    , _pNumFiles   :: Int
    , _pFiles      :: HM.HashMap B.ByteString PackFileT
    } deriving (Eq)

data DVisT = DVisT
    { _dvNumClusters :: Int
    , _dvBitOfs      :: V.Vector (Int, Int)
    }

data DMdlT = DMdlT
    { _dmIdent              :: Int
    , _dmVersion            :: Int
    , _dmSkinWidth          :: Int
    , _dmSkinHeight         :: Int
    , _dmFrameSize          :: Int
    , _dmNumSkins           :: Int
    , _dmNumXYZ             :: Int
    , _dmNumST              :: Int
    , _dmNumTris            :: Int
    , _dmNumGlCmds          :: Int
    , _dmNumFrames          :: Int
    , _dmOfsSkins           :: Int
    , _dmOfsST              :: Int
    , _dmOfsTris            :: Int
    , _dmOfsFrames          :: Int
    , _dmOfsGlCmds          :: Int
    , _dmOfsEnd             :: Int
    , _dmSkinNames          :: Maybe (V.Vector B.ByteString)
    , _dmSTVerts            :: Maybe (V.Vector DSTVertT)
    , _dmTriAngles          :: Maybe (V.Vector DTriangleT)
    , _dmGlCmds             :: Maybe (UV.Vector Word32)
    , _dmAliasFrames        :: Maybe (V.Vector DAliasFrameT)
    , _dmTextureCoordBufIdx :: Int
    , _dmVertexIndexBufIdx  :: Int
    , _dmCounts             :: UV.Vector Int32
    , _dmIndexElements      :: V.Vector (Int, Int)
    }

data DAliasFrameT = DAliasFrameT
    { _dafScale     :: V3 Float
    , _dafTranslate :: V3 Float
    , _dafName      :: B.ByteString
    , _dafVerts     :: UV.Vector Int
    } deriving Eq

data DSTVertT = DSTVertT
    { _dstvS :: Int16
    , _dstvT :: Int16
    }

data DTriangleT = DTriangleT
    { _dtIndexXYZ :: V3 Int16
    , _dtIndexST  :: V3 Int16
    }

data DSprFrameT = DSprFrameT
    { _dsfWidth   :: Int
    , _dsfHeight  :: Int
    , _dsfOriginX :: Int
    , _dsfOriginY :: Int
    , _dsfName    :: B.ByteString
    }

data DSpriteT = DSpriteT
    { _dsIdent     :: Int
    , _dsVersion   :: Int
    , _dsNumFrames :: Int
    , _dsFrames    :: V.Vector DSprFrameT
    }

data PackFileT = PackFileT
    { _pfName    :: B.ByteString
    , _pfFilePos :: Int
    , _pfFileLen :: Int
    } deriving (Eq)

data GLConfigT = GLConfigT
    { _glcRenderer         :: Int
    , _glcRendererString   :: B.ByteString
    , _glcVendorString     :: B.ByteString
    , _glcVersionString    :: B.ByteString
    , _glcExtensionsString :: B.ByteString
    , _glcAllowCds         :: Bool
    , _glcVersion          :: Float
    }

data GLStateT = GLStateT
    { _glsInverseIntensity        :: Float
    , _glsFullScreen              :: Bool
    , _glsPrevMode                :: Int
    , _glsD16To8Table             :: Maybe B.ByteString
    , _glsLightmapTextures        :: Int
    , _glsCurrentTextures         :: (Int, Int)
    , _glsCurrentTmu              :: Int
    , _glsCameraSeparation        :: Float
    , _glsStereoEnabled           :: Bool
    , _glsOriginalRedGammaTable   :: UV.Vector Word8
    , _glsOriginalGreenGammaTable :: UV.Vector Word8
    , _glsOriginalBlueGammaTable  :: UV.Vector Word8
    }

data MEdgeT = MEdgeT
    { _meV                :: (Word16, Word16)
    , _meCachedEdgeOffset :: Int
    }

data MModelT = MModelT
    { _mmMins      :: V3 Float
    , _mmMaxs      :: V3 Float
    , _mmOrigin    :: V3 Float
    , _mmRadius    :: Float
    , _mmHeadNode  :: Int
    , _mmVisLeafs  :: Int
    , _mmFirstFace :: Int
    , _mmNumFaces  :: Int
    }

data MVertexT = MVertexT
    { _mvPosition :: V3 Float
    }

data SfxT = SfxT
    { _sfxName                 :: B.ByteString
    , _sfxRegistrationSequence :: Int
    , _sfxCache                :: Maybe SfxCacheT
    , _sfxTrueName             :: B.ByteString
    , _sfxBufferId             :: Int
    , _sfxIsCached             :: Bool
    }

data LoopbackT = LoopbackT
    { _lMsgs :: V.Vector LoopMsgT
    , _lGet  :: Int
    , _lSend :: Int
    }

data LoopMsgT = LoopMsgT
    { _lmData    :: B.ByteString -- max len is Constants.maxMsgLen
    , _lmDataLen :: Int
    }

data SfxCacheT = SfxCacheT
    { _scLength    :: Int
    , _scLoopStart :: Int
    , _scSpeed     :: Int
    , _scWidth     :: Int
    , _scStereo    :: Int
    , _scData      :: B.ByteString
    }

data Socket -- = Socket Int

data CheatVarT = CheatVarT
    { _chvName  :: B.ByteString
    , _chvValue :: B.ByteString
    }

data MoveClipT = MoveClipT
    { _mcBoxMins     :: V3 Float
    , _mcBoxMaxs     :: V3 Float
    , _mcMins        :: V3 Float
    , _mcMaxs        :: V3 Float
    , _mcMins2       :: V3 Float
    , _mcMaxs2       :: V3 Float
    , _mcStart       :: V3 Float
    , _mcEnd         :: V3 Float
    , _mcTrace       :: TraceT
    , _mcPassEdict   :: Maybe (Ref EdictT)
    , _mcContentMask :: Int
    }

data LumpT = LumpT
    { _lFileOfs :: Int
    , _lFileLen :: Int
    }

data DPackHeaderT = DPackHeaderT
    { _dphIdent  :: Int -- IDPAKHEADER
    , _dphDirOfs :: Int
    , _dphDirLen :: Int
    }

data DAreaT = DAreaT
    { _daNumAreaPortals  :: Int
    , _daFirstAreaPortal :: Int
    }

data DBrushSideT = DBrushSideT
    { _dbsPlaneNum :: Word16
    , _dbsTexInfo  :: Int16
    }

data DBrushT = DBrushT
    { _dbFirstSide :: Int
    , _dbNumSides  :: Int
    , _dbContents  :: Int
    }

data DFaceT = DFaceT
    { _dfPlaneNum  :: Word16
    , _dfSide      :: Int16
    , _dfFirstEdge :: Int
    , _dfNumEdges  :: Int16
    , _dfTexInfo   :: Int16
    , _dfStyles    :: B.ByteString
    , _dfLightOfs  :: Int
    }

data DHeaderT = DHeaderT
    { _dhIdent   :: Int
    , _dhVersion :: Int
    , _dhLumps   :: V.Vector LumpT
    }

data DLeafT = DLeafT
    { _dlContents       :: Int
    , _dlCluster        :: Int16
    , _dlArea           :: Int16
    , _dlMins           :: V3 Int16
    , _dlMaxs           :: V3 Int16
    , _dlFirstLeafFace  :: Word16
    , _dlNumLeafFaces   :: Word16
    , _dlFirstLeafBrush :: Word16
    , _dlNumLeafBrushes :: Word16
    }

data DModelT = DModelT
    { _dmMins      :: V3 Float
    , _dmMaxs      :: V3 Float
    , _dmOrigin    :: V3 Float
    , _dmHeadNode  :: Int
    , _dmFirstFace :: Int
    , _dmNumFaces  :: Int
    }

data DNodeT = DNodeT
    { _dnPlaneNum  :: Int
    , _dnChildren  :: (Int, Int)
    , _dnMins      :: V3 Int16
    , _dnMaxs      :: V3 Int16
    , _dnFirstFace :: Word16
    , _dnNumFaces  :: Word16
    }

data DPlaneT = DPlaneT
    { _dpNormal :: V3 Float
    , _dpDist   :: Float
    , _dpType   :: Int
    }
