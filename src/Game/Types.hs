module Game.Types where

import Linear.V3 (V3)
import Linear.V4 (V4)
import Data.Int (Int8, Int16)
import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as UV

data ClientPersistant = ClientPersistant { clientPersistantUserInfo        :: B.ByteString
                                         , clientPersistantNetName         :: B.ByteString
                                         , clientPersistantHand            :: Int
                                         , clientPersistantConnected       :: Bool
                                         , clientPersistantHealth          :: Int
                                         , clientPersistantMaxHealth       :: Int
                                         , clientPersistantSavedFlags      :: Int
                                         , clientPersistantSelectedItem    :: Int
                                         , clientPersistantInventory       :: UV.Vector Int
                                         , clientPersistantMaxBullets      :: Int
                                         , clientPersistantMaxShells       :: Int
                                         , clientPersistantMaxRockets      :: Int
                                         , clientPersistantMaxGrenades     :: Int
                                         , clientPersistantMaxCells        :: Int
                                         , clientPersistantMaxSlugs        :: Int
                                         , clientPersistantWeapon          :: GItem
                                         , clientPersistantLastWeapon      :: GItem
                                         , clientPersistantPowerCubes      :: Int
                                         , clientPersistantScore           :: Int
                                         , clientPersistantGameHelpChanged :: Int
                                         , clientPersistantHelpChanged     :: Int
                                         , clientPersistantSpectator       :: Bool
                                         }

data ClientRespawn = ClientRespawn { clientRespawnCoopRespawn :: ClientPersistant
                                   , clientRespawnEnterFrame  :: Int
                                   , clientRespawnScore       :: Int
                                   , clientRespawnCmdAngles   :: V3 Float
                                   , clientRespawnSpectator   :: Bool
                                   }

data CmdAlias = CmdAlias { cmdAliasNext  :: Maybe CmdAlias
                         , cmdAliasName  :: B.ByteString
                         , cmdAliasValue :: B.ByteString
                         }

data CModel = CModel { cModelMins     :: V3 Float
                     , cModelMaxs     :: V3 Float
                     , cModelOrigin   :: V3 Float
                     , cModelHeadNode :: Int
                     }

data CPlane = CPlane { cPlaneNormal   :: V3 Float
                     , cPlaneDist     :: Float
                     , cPlaneType     :: Int8
                     , cPlaneSignBits :: Int8
                     , cPlanePad      :: (Int8, Int8)
                     }

data CSurface = CSurface { cSurfaceName  :: B.ByteString
                         , cSurfaceFlags :: Int
                         , cSurfaceValue :: Int
                         }

data CVar = CVar { cVarName :: B.ByteString
                 , cVarString :: B.ByteString
                 , cVarLatchedString :: B.ByteString
                 , cVarFlags :: Int
                 , cVarModified :: Bool
                 , cVarValue :: Float
                 , cVarNext :: Maybe CVar
                 }

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

-- TODO: function return types - is it really IO ?
data GameImport = GameImport { bprintf            :: Int -> B.ByteString -> IO ()
                             , dprintf            :: B.ByteString -> IO ()
                             , cprintf            :: Edict -> Int -> B.ByteString -> IO ()
                             , centerprintf       :: Edict -> B.ByteString -> IO ()
                             , sound              :: Edict -> Int -> Int -> Float -> Float -> Float -> IO ()
                             , positionedSound    :: V3 Float -> Edict -> Int -> Int -> Float -> Float -> Float -> IO ()
                             , configstring       :: Int -> B.ByteString -> IO ()
                             , error              :: B.ByteString -> IO ()
                             , error2             :: Int -> B.ByteString -> IO ()
                             , modelindex         :: B.ByteString -> IO Int
                             , soundindex         :: B.ByteString -> IO Int
                             , imageindex         :: B.ByteString -> IO Int
                             , setmodel           :: Edict -> B.ByteString -> IO ()
                             , trace              :: V3 Float -> V3 Float -> V3 Float -> V3 Float -> Edict -> Int -> IO Trace
                           --, pmove_t.PointContentsAdapter -- TODO: ???
                             , inPHS              :: V3 Float -> V3 Float -> IO Bool
                             , setAreaPortalState :: Int -> Bool -> IO ()
                             , areasConnected     :: Int -> Int -> IO Bool
                             , linkentity         :: Edict -> IO ()
                             , unlinkentity       :: Edict -> IO ()
                             , boxEdicts          :: V3 Float -> V3 Float -> UV.Vector Edict -> Int -> Int -> IO Int
                             , pmove              :: PMove -> IO ()
                             , multicast          :: V3 Float -> Int -> IO ()
                             , unicast            :: Edict -> Bool -> IO ()
                             , writeByte          :: Int -> IO ()
                             , writeShort         :: Int -> IO ()
                             , writeString        :: B.ByteString -> IO ()
                             , writePosition      :: V3 Float -> IO ()
                             , writeDir           :: V3 Float -> IO ()
                             , cvar               :: B.ByteString -> B.ByteString -> Int -> IO CVar
                             , cvarSet            :: B.ByteString -> B.ByteString -> IO CVar
                             , cvarForceset       :: B.ByteString -> B.ByteString -> IO CVar
                             , argc               :: IO Int
                             , argv               :: Int -> B.ByteString
                             , args               :: IO B.ByteString
                             , addCommandString   :: B.ByteString -> IO ()
                             }

data GameLocals = GameLocals { gameLocalsHelpMessage1 :: B.ByteString
                             , gameLocalsHelpMessage2 :: B.ByteString
                             , gameLocalsHelpChanged  :: Int
                             , gameLocalsClients      :: UV.Vector GClient
                             , gameLocalsSpawnPoint   :: B.ByteString
                             , gameLocalsMaxClients   :: Int
                             , gameLocalsMaxEntities  :: Int
                             , gameLocalsServerFlags  :: Int
                             , gameLocalsNumItems     :: Int
                             , gameLocalsAutosaved    :: Bool
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

data GItemArmor = GItemArmor { gItemArmorBaseCount        :: Int
                             , gItemArmorMaxCount         :: Int
                             , gItemArmorNormalProtection :: Float
                             , gItemArmorEnergyProteciton :: Float
                             , gItemArmorArmor            :: Int
                             }

data GItem = GItem { gItemId              :: Int
                   , gItemClassName       :: B.ByteString
                   , gItemPickup          :: IO () -- TODO: ???
                   , gItemUse             :: IO () -- TODO: ???
                   , gItemDrop            :: IO () -- TODO: ???
                   , gItemWeaponThink     :: IO () -- TODO: ???
                   , gItemPickupSound     :: B.ByteString
                   , gItemWorldModel      :: B.ByteString
                   , gItemWorldModelFlags :: Int
                   , gItemViewModel       :: B.ByteString
                   , gItemIcon            :: B.ByteString
                   , gItemPickupName      :: B.ByteString
                   , gItemCountWidth      :: Int
                   , gItemQuantity        :: Int
                   , gItemAmmo            :: B.ByteString
                   , gItemFlags           :: Int
                   , gItemWeaponModel     :: Int
                   , gItemInfo            :: GItemArmor
                   , gItemTag             :: Int
                   , gItemPrecaches       :: B.ByteString
                   , gItemIndex           :: Int
                   }

data LevelLocals = LevelLocals { levelLocalsFrameNum             :: Int
                               , levelLocalsTime                 :: Float
                               , levelLocalsLevelName            :: B.ByteString
                               , levelLocalsMapName              :: B.ByteString
                               , levelLocalsNextMap              :: B.ByteString
                               , levelLocalsIntermissionTime     :: Float
                               , levelLocalsChangeMap            :: B.ByteString
                               , levelLocalsExitIntermission     :: Bool
                               , levelLocalsIntermissionOrigin   :: V3 Float
                               , levelLocalsIntermissionAngle    :: V3 Float
                               , levelLocalsSightClient          :: Edict
                               , levelLocalsSightEntity          :: Edict
                               , levelLocalsSightEntityFrameNum  :: Int
                               , levelLocalsSoundEntity          :: Edict
                               , levelLocalsSoundEntityFrameNum  :: Int
                               , levelLocalsSound2Entity         :: Edict
                               , levelLocalsSound2EntityFrameNum :: Int
                               , levelLocalsPicHealth            :: Int
                               , levelLocalsTotalSecrets         :: Int
                               , levelLocalsFoundSecrets         :: Int
                               , levelLocalsTotalGoals           :: Int
                               , levelLocalsFoundGoals           :: Int
                               , levelLocalsTotalMonsters        :: Int
                               , levelLocalsKilledMonsters       :: Int
                               , levelLocalsCurrentEntity        :: Edict
                               , levelLocalsBodyQue              :: Int
                               , levelLocalsPowerCubes           :: Int
                               }

data Link = Link { linkPrev :: Maybe Link
                 , linkNext :: Maybe Link
                 --, object :: ??? -- TODO: is it needed?
                 }

data MapSurface = MapSurface { mapSurfaceCSurface :: CSurface
                             , mapSurfaceRName    :: B.ByteString
                             }

data MFrame = MFrame { mFrameAI    :: IO () -- TODO: ???
                     , mFrameDist  :: Float
                     , mFrameThink :: IO () -- TODO: ???
                     }

data MMove = MMove { mMoveFirstFrame :: Int
                   , mMoveLastFrame  :: Int
                   , mMoveFrame      :: UV.Vector MFrame
                   , mMoveEndFunc    :: IO () -- TODO: ???
                   }

data MonsterInfo = MonsterInfo { monsterInfoCurrentMove     :: MMove
                               , monsterInfoAIFlags         :: Int
                               , monsterInfoNextFrame       :: Int
                               , monsterInfoScale           :: Float
                               , monsterInfoStand           :: IO () -- TODO: ???
                               , monsterInfoIdle            :: IO () -- TODO: ???
                               , monsterInfoSearch          :: IO () -- TODO: ???
                               , monsterInfoWalk            :: IO () -- TODO: ???
                               , monsterInfoRun             :: IO () -- TODO: ???
                               , monsterInfoDodge           :: IO () -- TODO: ???
                               , monsterInfoAttack          :: IO () -- TODO: ???
                               , monsterInfoMelee           :: IO () -- TODO: ???
                               , monsterInfoSight           :: IO () -- TODO: ???
                               , monsterInfoCheckAttack     :: IO () -- TODO: ???
                               , monsterInfoPauseTime       :: Float
                               , monsterInfoAttackFinished  :: Float
                               , monsterInfoSavedGoal       :: V3 Float
                               , monsterInfoSearchTime      :: Float
                               , monsterInfoTrailTime       :: Float
                               , monsterInfoLastSighting    :: V3 Float
                               , monsterInfoAttackState     :: Int
                               , monsterInfoLefty           :: Int
                               , monsterInfoIdleTime        :: Float
                               , monsterInfoLinkCount       :: Int
                               , monsterInfoPowerArmorType  :: Int
                               , monsterInfoPowerArmorPower :: Int
                               }

data MoveInfo = MoveInfo { moveInfoStartOrigin       :: V3 Float
                         , moveInfoStartAngles       :: V3 Float
                         , moveInfoEndOrigin         :: V3 Float
                         , moveInfoEndAngles         :: V3 Float
                         , moveInfoSoundStart        :: Int
                         , moveInfoSoundMiddle       :: Int
                         , moveInfoSoundEnd          :: Int
                         , moveInfoAccel             :: Float
                         , moveInfoSpeed             :: Float
                         , moveInfoDecel             :: Float
                         , moveInfoDistance          :: Float
                         , moveInfoWait              :: Float
                         , moveInfoState             :: Int
                         , moveInfoDir               :: V3 Float
                         , moveInfoCurrentSpeed      :: Float
                         , moveInfoMoveSpeed         :: Float
                         , moveInfoNextSpeed         :: Float
                         , moveInfoRemainingDistance :: Float
                         , moveInfoDecelDistance     :: Float
                         , moveInfoEndFunc           :: IO () -- TODO: ??
                         }

data PlayerState = PlayerState { playerStatePMoveState :: PMoveState
                               , playerStateViewAngles :: V3 Float
                               , playerStateViewOffset :: V3 Float
                               , playerStateKickAngles :: V3 Float
                               , playerStateGunAngles  :: V3 Float
                               , playerStateGunOffset  :: V3 Float
                               , playerStateGunIndex   :: Int
                               , playerStateGunFrame   :: Int
                               , playerStateBlend      :: V4 Float
                               , playerStateFOV        :: Float
                               , playerStateRDFlags    :: Int
                               , playerStateStats      :: UV.Vector Int16
                               , playerStatePrototype  :: PlayerState
                               }

data PMoveState = PMoveState { pMoveStatePMType      :: Int
                             , pMoveStateOrigin      :: V3 Float
                             , pMoveStateVelocity    :: V3 Float
                             , pMoveStatePMFlags     :: Int8
                             , pMoveStatePMTime      :: Int8
                             , pMoveStateGravity     :: Int16
                             , pMoveStateDeltaAngles :: V3 Int16
                             , pMoveStatePrototype   :: PMoveState
                             }

data PMove = PMove { pMoveState         :: PMoveState
                   , pMoveCmd           :: UserCmd
                   , pMoveSnapInitial   :: Bool
                   , pMoveNumTouch      :: Int
                   , pMoveTouchEnts     :: UV.Vector Edict
                   , pMoveViewAngles    :: V3 Float
                   , pMoveViewHeight    :: Float
                   , pMoveMins          :: V3 Float
                   , pMoveMaxs          :: V3 Float
                   , pMoveGroundEntity  :: Edict
                   , pMoveWaterType     :: Int
                   , pMoveWaterLevel    :: Int
                   , pMoveTrace         :: IO () -- TODO: ???
                   , pMovePointContents :: IO () -- TODO: ???
                   {-
                   TODO:
                   PMF_DUCKED = 1
                   PMF_JUMP_HELD = 2
                   PMF_ON_GROUND = 4
                   PMF_TIME_WATERJUMP = 8
                   PMF_TIME_LAND = 16
                   PMF_TIME_TELEPORT = 32
                   PMF_NO_PREDICTION = 64
                   -}
                   }

data Pushed = Pushed { pushedEnt      :: Edict
                     , pushedOrigin   :: V3 Float
                     , pushedAngles   :: V3 Float
                     , pushedDeltaYaw :: Float
                     }

data Spawn = Spawn { spawnName :: B.ByteString
                   , spawnSpawn :: IO () -- TODO: ???
                   }

data SpawnTemp = SpawnTemp { spawnTempSky       :: B.ByteString
                           , spawnTempSkyRotate :: Float
                           , spawnTempSkyAxis   :: V3 Float
                           , spawnNextMap       :: B.ByteString
                           , spawnTempLip       :: Int
                           , spawnTempDistance  :: Int
                           , spawnTempHeight    :: Int
                           , spawnTempNoise     :: B.ByteString
                           , spawnTempPauseTime :: Float
                           , spawnTempItem      :: B.ByteString
                           , spawnTempGravity   :: B.ByteString
                           , spawnTempMinYaw    :: Float
                           , spawnTempMaxYaw    :: Float
                           , spawnTempMinPitch  :: Float
                           , spawnTempMaxPitch  :: Float
                           }

data Trace = Trace { traceAllSolid   :: Bool
                   , traceStartSolid :: Bool
                   , traceFraction   :: Float
                   , traceEndPos     :: V3 Float
                   , tracePlane      :: CPlane
                   , traceSurface    :: CSurface
                   , traceContents   :: Int
                   , traceEnt        :: Edict
                   }

data UserCmd = UserCmd { userCmdMsec        :: Int8
                       , userCmdButtons     :: Int8
                       , userCmdAngles      :: V3 Int16
                       , userCmdForwardMove :: Int16
                       , userCmdSideMove    :: Int16
                       , userCmdUpMove      :: Int16
                       , userCmdImpulse     :: Int8
                       , userCmdLightLevel  :: Int8
                       }
