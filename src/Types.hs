{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
module Types where

import           Control.Monad.State (State,StateT,MonadState,MonadIO,lift,get,put,liftIO)
import           Control.Monad.Coroutine (Coroutine(..), suspend)
import qualified Data.ByteString as B
import qualified Data.HashMap.Lazy as HM
import           Data.Int (Int8, Int16)
import           Data.Sequence (Seq)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as UV
import           Data.Word (Word8)
import           Linear (V3, V4)
import           Network.Socket (HostAddress)
import           System.IO (Handle)
import           System.Random (StdGen)

type Quake = Coroutine IORequest (State QuakeState)

instance MonadState QuakeState Quake where
  get = lift get
  put = lift . put

type QuakeIO = StateT QuakeIOState IO

io :: MonadIO m => IO a -> m a
io = liftIO

data QuakeState = QuakeState
  { _globals    :: Globals
  , _comGlobals :: ComGlobals
  , _cmdGlobals :: CmdGlobals
  , _keyGlobals :: KeyGlobals
  , _fsGlobals  :: FSGlobals
  , _svGlobals  :: SVGlobals
  }

data QuakeIOState = QuakeIOState
  { _ioGEdicts :: MV.IOVector EdictT
  }

data IORequest x
  = forall a. RunIO (QuakeIO a) (a -> x)

instance Functor IORequest where
  fmap f (RunIO x g) = RunIO x (f . g)

request :: Monad m => QuakeIO a -> Coroutine IORequest m a
request x = suspend (RunIO x return)

newtype EdictRef = EdictRef Int deriving (Eq, Show, Ord)
newtype ClientRef = ClientRef Int deriving Eq
newtype GClientRef = GClientRef Int deriving Eq
newtype CModelRef = CModelRef Int deriving Eq
newtype LinkRef = LinkRef Int deriving Eq
newtype GItemRef = GItemRef Int deriving Eq

data Globals = Globals
  { _gCurTime          :: Int
  , _gCmdWait          :: Bool
  , _gAliasCount       :: Int
  , _gServerState      :: Int
  , _gNetMessage       :: SizeBufT
  , _gCmdText          :: SizeBufT
  , _gCmdAlias         :: Seq CmdAliasT
  , _gCls              :: ClientStaticT
  , _gUserInfoModified :: Bool
  , _gCVars            :: HM.HashMap B.ByteString CVarT
  , _gKeyBindings      :: V.Vector (Maybe B.ByteString)
  , _gKeyLines         :: V.Vector B.ByteString
  , _gKeyLinePos       :: Int
  , _gRnd              :: StdGen
  }

data ComGlobals = ComGlobals
  { _cgComArgc :: Int
  , _cgComArgv :: V.Vector B.ByteString
  }

data CmdGlobals = CmdGlobals
  { _cgCmdFunctions :: Seq CmdFunctionT
  , _cgCmdArgc      :: Int
  , _cgCmdArgv      :: V.Vector B.ByteString
  , _cgCmdArgs      :: B.ByteString
  }
  
data KeyGlobals = KeyGlobals
  { _kgAnyKeyDown  :: Int
  , _kgKeyWaiting  :: Int
  , _kgHistoryLine :: Int
  , _kgShiftDown   :: Bool
  , _kgKeyRepeats  :: UV.Vector Int
  , _kgMenuBound   :: UV.Vector Bool
  , _kgConsoleKeys :: UV.Vector Bool
  , _kgKeyNames    :: V.Vector (Maybe B.ByteString)
  }

data FSGlobals = FSGlobals
  { _fsGameDir         :: B.ByteString
  , _fsUserDir         :: B.ByteString
  , _fsLinks           :: Seq FileLinkT
  , _fsSearchPaths     :: [SearchPathT]
  , _fsBaseSearchPaths :: [SearchPathT]
  , _fsFileFromPak     :: Int
  }

data SVGlobals = SVGlobals
  { _svMasterAdr    :: V.Vector NetAdrT
  , _svClient       :: Maybe ClientRef
  , _svServer       :: ServerT
  , _svServerStatic :: ServerStaticT
  , _svPlayer       :: Maybe EdictRef
  , _svFirstMap     :: B.ByteString
  , _svMsgBuf       :: B.ByteString
  , _svNumAreaNodes :: Int
  , _svAreaNodes    :: V.Vector AreaNodeT
  , _svAreaMins     :: V3 Float
  , _svAreaMaxs     :: V3 Float
  , _svAreaList     :: V.Vector EdictRef
  , _svAreaCount    :: Int
  , _svAreaMaxCount :: Int
  , _svAreaType     :: Int
  , _svLeafs        :: UV.Vector Int
  , _svClusters     :: UV.Vector Int
  , _svTouch        :: V.Vector EdictRef
  , _svTouchList    :: V.Vector EdictRef
  , _svLinks        :: V.Vector LinkT
  , _svMsg          :: SizeBufT
  , _svLeafsTmp     :: UV.Vector Int
  , _svFatPVS       :: UV.Vector Word8
  }
  
data CVarT = CVarT
  { _cvName          :: B.ByteString
  , _cvString        :: B.ByteString
  , _cvLatchedString :: Maybe B.ByteString
  , _cvFlags         :: Int
  , _cvModified      :: Bool
  , _cvValue         :: Float
  } deriving (Eq)

data SizeBufT = SizeBufT
  { _sbAllowOverflow :: Bool
  , _sbOverflowed    :: Bool
  , _sbData          :: B.ByteString
  , _sbMaxSize       :: Int
  , _sbCurSize       :: Int
  , _sbReadCount     :: Int
  }

data CmdFunctionT = CmdFunctionT
  { _cfName     :: B.ByteString
  , _cfFunction :: Maybe XCommandT
  }

data CmdAliasT = CmdAliasT
  { _caName  :: B.ByteString
  , _caValue :: B.ByteString
  } deriving (Eq)

data SearchPathT = SearchPathT
  { _spFilename :: B.ByteString
  , _spPack     :: Maybe PackT
  } deriving (Eq)

data PackT = PackT
  { _pFilename   :: B.ByteString
  , _pHandle     :: Maybe Handle
  , _pBackBuffer :: B.ByteString
  , _pNumFiles   :: Int
  , _pFiles      :: HM.HashMap B.ByteString PackFileT
  } deriving (Eq)

data PackFileT = PackFileT
  { _pfName    :: B.ByteString
  , _pfFilePos :: Int
  , _pfFileLen :: Int
  } deriving (Eq)

data FileLinkT = FileLinkT
  { _flFrom       :: B.ByteString
  , _flFromLength :: Int
  , _flTo         :: B.ByteString
  }
  
data XCommandT = XCommandT
  { _xcName :: B.ByteString
  , _xcCmd  :: Quake ()
  }

instance Eq XCommandT where
  x == y = _xcName x == _xcName y

data NetAdrT = NetAdrT
  { _naType :: Int
  , _naPort :: Int
  , _naIP   :: Maybe HostAddress
  }

data ServerT = ServerT
  { _sState         :: Int
  , _sAttractLoop   :: Bool
  , _sLoadGame      :: Bool
  , _sTime          :: Int
  , _sFrameNum      :: Int
  , _sName          :: B.ByteString
  , _sModels        :: V.Vector CModelRef
  , _sConfigStrings :: V.Vector B.ByteString
  , _sBaselines     :: V.Vector EntityStateT
  , _sMulticast     :: SizeBufT
  , _sMulticastBuf  :: B.ByteString
  , _sDemoFile      :: Maybe Handle
  , _sTimeDemo      :: Int
  }

data ServerStaticT = ServerStaticT
  { _ssInitialized        :: Bool
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

data AreaNodeT = AreaNodeT
  { _anAxis          :: Int
  , _anDist          :: Float
  , _anChildren      :: (Maybe Int, Maybe Int) -- indexes to svGlobals.svAreaNodes IMPROVE: newtype?
  , _anTriggerEdicts :: LinkRef
  , _anSolidEdicts   :: LinkRef
  }

data LinkT = LinkT
  { _lIndex :: Int
  , _lPrev  :: Maybe LinkRef
  , _lNext  :: Maybe LinkRef
  , _lEdict :: Maybe EdictRef
  }

data ChallengeT = ChallengeT
  { _chAdr       :: NetAdrT
  , _chChallenge :: Int
  , _chTime      :: Int
  }

data EntityStateT = EntityStateT
  { _esNumber         :: Int
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
  , _esSurroundingEnt :: Maybe EdictRef
  }

data ClientT = ClientT
  { _cState         :: Int
  , _cUserInfo      :: B.ByteString
  , _cLastFrame     :: Int
  , _cLastCmd       :: UserCmdT
  , _cCommandMsec   :: Int
  , _cFrameLatency  :: UV.Vector Int
  , _cPing          :: Int
  , _cMessageSize   :: UV.Vector Int
  , _cRate          :: Int
  , _cSurpressCount :: Int
  , _cEdict         :: Maybe EdictRef
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
  , _ncReliableLength               :: Int
  , _ncReliableBuf                  :: B.ByteString
  }

data ClientFrameT = ClientFrameT
  { _cfAreaBytes   :: Int
  , _cfAreaBits    :: VS.Vector Word8
  , _cfPlayerState :: PlayerStateT
  , _cfNumEntities :: Int
  , _cfFirstEntity :: Int
  , _cfSentTime    :: Int
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

data PMoveT = PMoveT
  { _pmState         :: PMoveStateT
  , _pmCmd           :: UserCmdT
  , _pmSnapInitial   :: Bool
  , _pmNumTouch      :: Int
  , _pmTouchEnts     :: V.Vector EdictRef
  , _pmViewAngles    :: V3 Float
  , _pmViewHeight    :: Float
  , _pmMins          :: V3 Float
  , _pmMaxs          :: V3 Float
  , _pmGroundEntity  :: Maybe EdictRef
  , _pmWaterType     :: Int
  , _pmWaterLevel    :: Int
  , _pmTrace         :: V3 Float -> V3 Float -> V3 Float -> V3 Float -> Quake (Maybe TraceT)
  , _pmPointContents :: V3 Float -> Quake Int
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

data TraceT = TraceT
  { _tAllSolid   :: Bool
  , _tStartSolid :: Bool
  , _tFraction   :: Float
  , _tEndPos     :: V3 Float
  , _tPlane      :: CPlaneT
  , _tSurface    :: Maybe CSurfaceT
  , _tContents   :: Int
  , _tEnt        :: Maybe EdictRef
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

data EdictT =
  EdictT { _eEntityState           :: EntityStateT
         , _eInUse                 :: Bool
         , _eClassName             :: B.ByteString
         , _eLinkCount             :: Int
         , _eArea                  :: LinkRef
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
         , _eTargetEnt             :: Maybe EdictRef
         , _eGoalEntity            :: Maybe EdictRef
         , _eMoveTarget            :: Maybe EdictRef
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
         , _eChain                 :: Maybe EdictRef
         , _eEnemy                 :: Maybe EdictRef
         , _eOldEnemy              :: Maybe EdictRef
         , _eActivator             :: Maybe EdictRef
         , _eGroundEntity          :: Maybe EdictRef
         , _eTeamChain             :: Maybe EdictRef
         , _eTeamMaster            :: Maybe EdictRef
         , _eMyNoise               :: Maybe EdictRef
         , _eMyNoise2              :: Maybe EdictRef
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
         , _eItem                  :: Maybe GItemRef
         , _eMoveInfo              :: MoveInfoT
         , _eMonsterInfo           :: MonsterInfoT
         , _eClient                :: Maybe GClientRef
         , _eOwner                 :: Maybe EdictRef
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

data MoveInfoT = MoveInfoT
  { _miStartOrigin       :: V3 Float
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

data MonsterInfoT = MonsterInfoT
  { _miCurrentMove     :: Maybe MMoveT
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

data MMoveT = MMoveT
  { _mmId         :: B.ByteString -- to check for equality
  , _mmFirstFrame :: Int
  , _mmLastFrame  :: Int
  , _mmFrame      :: V.Vector MFrameT
  , _mmEndFunc    :: Maybe EntThink
  }

data MFrameT = MFrameT
  { _mfAI    :: Maybe AI
  , _mfDist  :: Float
  , _mfThink :: Maybe EntThink
  }

data GClientT = GClientT
  { _gcPlayerState        :: PlayerStateT
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
  , _gcNewWeapon          :: Maybe GItemRef
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
  , _gcChaseTarget        :: Maybe EdictRef
  , _gcUpdateChase        :: Bool
  , _gcIndex              :: Int
  }

data ClientPersistantT = ClientPersistantT
  { _cpUserInfo        :: B.ByteString
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
  , _cpWeapon          :: Maybe GItemRef
  , _cpLastWeapon      :: Maybe GItemRef
  , _cpPowerCubes      :: Int
  , _cpScore           :: Int
  , _cpGameHelpChanged :: Int
  , _cpHelpChanged     :: Int
  , _cpSpectator       :: Bool
  }

data ClientRespawnT = ClientRespawnT
  { _crCoopRespawn :: ClientPersistantT
  , _crEnterFrame  :: Int
  , _crScore       :: Int
  , _crCmdAngles   :: V3 Float
  , _crSpectator   :: Bool
  }


data CModelT = CModelT
  { _cmMins     :: V3 Float
  , _cmMaxs     :: V3 Float
  , _cmOrigin   :: V3 Float
  , _cmHeadNode :: Int
  }

data GItemT = GItemT
  { _giClassName       :: B.ByteString
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

data GItemArmorT = GItemArmorT
  { _giaBaseCount        :: Int
  , _giaMaxCount         :: Int
  , _giaNormalProtection :: Float
  , _giaEnergyProtection :: Float
  , _giaArmor            :: Int
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

data AI = AI
  { _aiId :: B.ByteString
  , _aiAi :: EdictRef -> Float -> Quake ()
  }

data EntInteract = EntInteract
  { _eiId       :: B.ByteString
  , _eiInteract :: EdictRef -> EdictRef -> Quake Bool
  }

data EntThink = EntThink
  { _ethId    :: B.ByteString
  , _ethThink :: EdictRef -> Quake Bool
  }

data EntBlocked = EntBlocked
  { _ebId      :: B.ByteString
  , _ebBlocked :: EdictRef -> EdictRef -> Quake ()
  }

data EntDodge = EntDodge
  { _edoId    :: B.ByteString
  , _edoDodge :: EdictRef -> EdictRef -> Float -> Quake ()
  }

data EntTouch = EntTouch
  { _etId    :: B.ByteString
  , _etTouch :: EdictRef -> EdictRef -> CPlaneT -> Maybe CSurfaceT -> Quake ()
  }

data EntUse = EntUse
  { _euId  :: B.ByteString
  , _euUse :: EdictRef -> Maybe EdictRef -> Maybe EdictRef -> Quake ()
  }

data EntPain = EntPain
  { _epId   :: B.ByteString
  , _epPain :: EdictRef -> EdictRef -> Float -> Int -> Quake ()
  }

data EntDie = EntDie
  { _edId  :: B.ByteString
  , _edDie :: EdictRef -> EdictRef -> EdictRef -> Int -> V3 Float -> Quake ()
  }

data ItemUse = ItemUse
  { _iuId  :: B.ByteString
  , _iuUse :: EdictRef -> GItemRef -> Quake ()
  }

data ItemDrop = ItemDrop
  { _idId   :: B.ByteString
  , _idDrop :: EdictRef -> GItemRef -> Quake ()
  }