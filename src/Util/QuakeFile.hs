{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
module Util.QuakeFile ( QuakeFile
                      , open
                      , close
                      , writeString
                      , readString
                      , writeInt
                      , readInt
                      , writeEdict
                      , writeLevelLocals
                      , readLevelLocals
                      , writeGClient
                      , readGClient
                      , writeGameLocals
                      , readGameLocals
                      ) where

import Control.Lens ((^.))
import Control.Monad (void)
import Data.Binary.Get
import Data.Binary.Put
import Data.Functor ((<$>))
import Data.Int (Int8, Int16)
import Linear (V3(..), V4(..))
import System.IO (openFile, IOMode(ReadWriteMode), Handle, hClose)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import Game.GameLocalsT
import Game.EntityStateT
import Game.MFrameT
import Game.MMoveT
import Game.MonsterInfoT
import Game.MoveInfoT
import Game.ClientRespawnT
import Game.ClientPersistantT
import Game.GClientT
import Game.PlayerStateT
import Types
import Game.PMoveStateT
import QuakeState
import Game.EdictT
import Game.LevelLocalsT
import Util.Binary
import qualified Constants

data QuakeFile = QuakeFile Handle

open :: B.ByteString -> IO QuakeFile
open name = QuakeFile <$> openFile (BC.unpack name) ReadWriteMode

close :: QuakeFile -> IO ()
close (QuakeFile h) = hClose h -- IMPROVE: catch exception?

writeString :: QuakeFile -> Maybe B.ByteString -> IO ()
writeString (QuakeFile h) Nothing = do
    print "writeString = Nothing"
    BL.hPut h $ runPut $ putInt (-1)
writeString (QuakeFile h) (Just str) = do
    print ("writeString = " `B.append` str)
    BL.hPut h $ runPut $ do
      putInt (B.length str)
      putByteString str

readString :: QuakeFile -> IO (Maybe B.ByteString)
readString (QuakeFile h) = do
    stringSize <- BL.hGet h 4
    if BL.length stringSize /= 4
      then do
        print "readString FAILURE"
        return Nothing
      else do
        let len :: Int = runGet getInt stringSize
        if | len == -1 -> do
               print "readString = Nothing"
               return Nothing
           | len == 0 -> do
               print "readString = \"\""
               return (Just "")
           | otherwise -> do
               str <- B.hGet h len
               if B.length str /= len
                 then do
                   print "readString = FAILURE"
                   return Nothing
                 else do
                   print ("readString = " `B.append` str)
                   return $ Just str

writeInt :: QuakeFile -> Int -> IO ()
writeInt (QuakeFile h) num = do
    print ("writeInt = " ++ show num)
    BL.hPut h $ runPut $ putInt num

readInt :: QuakeFile -> IO Int
readInt (QuakeFile h) = do
    num <- BL.hGet h 4
    print ("readInt = " ++ show (runGet getInt num))
    return (runGet getInt num)

writeShort :: QuakeFile -> Int16 -> IO ()
writeShort (QuakeFile h) num = do
    print ("writeShort = " ++ show num)
    BL.hPut h $ runPut $ putInt16 num

readShort :: QuakeFile -> IO Int16
readShort (QuakeFile h) = do
    num <- BL.hGet h 2
    print ("readShort = " ++ show (runGet getInt16 num))
    return (runGet getInt16 num)

writeByte :: QuakeFile -> Int8 -> IO ()
writeByte (QuakeFile h) num = do
    print ("writeByte = " ++ show num)
    BL.hPut h $ runPut $ putWord8 (fromIntegral num)

readByte :: QuakeFile -> IO Int8
readByte (QuakeFile h) = do
    num <- BL.hGet h 1
    print ("readByte = " ++ show (runGet getWord8 num))
    return (fromIntegral $ runGet getWord8 num)

writeFloat :: QuakeFile -> Float -> IO ()
writeFloat (QuakeFile h) num = do
    print ("writeFloat = " ++ show num)
    BL.hPut h $ runPut $ putFloat num
    
readFloat :: QuakeFile -> IO Float
readFloat (QuakeFile h) = do
    num <- BL.hGet h 4
    print ("readFloat = " ++ show (runGet getFloat num))
    return (runGet getFloat num)

writeBool :: QuakeFile -> Bool -> IO ()
writeBool (QuakeFile h) p = do
    print ("writeBool = " ++ show p)
    BL.hPut h $ runPut $ putBool p

readBool :: QuakeFile -> IO Bool
readBool (QuakeFile h) = do
    p <- BL.hGet h 1
    print ("readBool = " ++ show (runGet getBool p))
    return (runGet getBool p)

writeEdictRef :: QuakeFile -> Maybe (Ref EdictT) -> IO ()
writeEdictRef saveFile Nothing = writeInt saveFile (-1)
writeEdictRef saveFile (Just (Ref idx)) = writeInt saveFile idx

readEdictRef :: QuakeFile -> IO (Maybe (Ref EdictT))
readEdictRef (QuakeFile h) = do
    num <- BL.hGet h 4
    let idx = runGet getInt num
    
    return $ if idx == -1
               then Nothing
               else Just (Ref idx) -- IMPROVE: if (i > GameBase.g_edicts.length) {
                                   --              Com.DPrintf("jake2: illegal edict num:" + i + "\n");
                                   --              return null;
                                   --          }

writeVector :: QuakeFile -> V3 Float -> IO ()
writeVector saveFile (V3 a b c) = do
    writeFloat saveFile a
    writeFloat saveFile b
    writeFloat saveFile c

readVector :: QuakeFile -> IO (V3 Float)
readVector saveFile = do
    a <- readFloat saveFile
    b <- readFloat saveFile
    c <- readFloat saveFile
    return (V3 a b c)

writeVectorShort :: QuakeFile -> V3 Int16 -> IO ()
writeVectorShort saveFile (V3 a b c) = do
    writeShort saveFile a
    writeShort saveFile b
    writeShort saveFile c

readVectorShort :: QuakeFile -> IO (V3 Int16)
readVectorShort saveFile = do
    a <- readShort saveFile
    b <- readShort saveFile
    c <- readShort saveFile
    return (V3 a b c)

writeLevelLocals :: QuakeFile -> LevelLocalsT -> IO ()
writeLevelLocals saveFile level = do
    print "writeLevelLocals"
    writeInt      saveFile (level^.llFrameNum)
    writeFloat    saveFile (level^.llTime)
    writeString   saveFile (Just $ level^.llLevelName)
    writeString   saveFile (Just $ level^.llMapName)
    writeString   saveFile (Just $ level^.llNextMap)
    writeFloat    saveFile (level^.llIntermissionTime)
    writeString   saveFile (Just $ level^.llChangeMap)
    writeBool     saveFile (level^.llExitIntermission)
    writeVector   saveFile (level^.llIntermissionOrigin)
    writeVector   saveFile (level^.llIntermissionAngle)
    writeEdictRef saveFile (level^.llSightClient)
    
    writeEdictRef saveFile (level^.llSightEntity)
    writeInt      saveFile (level^.llSightEntityFrameNum)
    
    writeEdictRef saveFile (level^.llSoundEntity)
    writeInt      saveFile (level^.llSoundEntityFrameNum)
    writeEdictRef saveFile (level^.llSound2Entity)
    writeInt      saveFile (level^.llSound2EntityFrameNum)
    
    writeInt      saveFile (level^.llPicHealth)
    
    writeInt      saveFile (level^.llTotalSecrets)
    writeInt      saveFile (level^.llFoundSecrets)
    
    writeInt      saveFile (level^.llTotalGoals)
    writeInt      saveFile (level^.llFoundGoals)
    writeInt      saveFile (level^.llTotalMonsters)
    writeInt      saveFile (level^.llKilledMonsters)
    
    writeEdictRef saveFile (level^.llCurrentEntity)
    writeInt      saveFile (level^.llBodyQue) -- dead bodies
    writeInt      saveFile (level^.llPowerCubes) -- ugly necessity for coop
    
    -- rst's checker :-)
    writeInt      saveFile 4711

readLevelLocals :: QuakeFile -> IO (LevelLocalsT)
readLevelLocals saveFile = do
    putStrLn "QuakeFile.readLevelLocals" >> undefined -- TODO

writeEdict :: QuakeFile -> EdictT -> IO ()
writeEdict saveFile edict = do
    print "writeEdict"
    writeEntityState saveFile (edict^.eEntityState)
    writeBool        saveFile (edict^.eInUse)
    writeInt         saveFile (edict^.eLinkCount)
    writeInt         saveFile (edict^.eNumClusters)
    
    writeInt         saveFile 9999
    
    writeInt         saveFile Constants.maxEntClusters
    UV.mapM_ (writeInt saveFile) (edict^.eClusterNums)
    
    writeInt         saveFile (edict^.eHeadNode)
    writeInt         saveFile (edict^.eAreaNum)
    writeInt         saveFile (edict^.eAreaNum2)
    writeInt         saveFile (edict^.eSvFlags)
    writeVector      saveFile (edict^.eMins)
    writeVector      saveFile (edict^.eMaxs)
    writeVector      saveFile (edict^.eAbsMin)
    writeVector      saveFile (edict^.eAbsMax)
    writeVector      saveFile (edict^.eSize)
    writeInt         saveFile (edict^.eSolid)
    writeInt         saveFile (edict^.eClipMask)
    
    writeInt         saveFile (edict^.eMoveType)
    writeInt         saveFile (edict^.eFlags)
    
    writeString      saveFile (edict^.eiModel)
    writeFloat       saveFile (edict^.eFreeTime)
    writeString      saveFile (edict^.eMessage)
    writeString      saveFile (Just $ edict^.eClassName)
    writeInt         saveFile (edict^.eSpawnFlags)
    writeFloat       saveFile (edict^.eTimeStamp)
    
    writeFloat       saveFile (edict^.eAngle)
    
    writeString      saveFile (edict^.eTarget)
    writeString      saveFile (edict^.eTargetName)
    writeString      saveFile (edict^.eKillTarget)
    writeString      saveFile (edict^.eTeam)
    writeString      saveFile (edict^.ePathTarget)
    writeString      saveFile (edict^.eDeathTarget)
    writeString      saveFile (edict^.eCombatTarget)
    
    writeEdictRef    saveFile (edict^.eTargetEnt)
    
    writeFloat       saveFile (edict^.eSpeed)
    writeFloat       saveFile (edict^.eAccel)
    writeFloat       saveFile (edict^.eDecel)
    
    writeVector      saveFile (edict^.eMoveDir)
    
    writeVector      saveFile (edict^.ePos1)
    writeVector      saveFile (edict^.ePos2)
    
    writeVector      saveFile (edict^.eVelocity)
    writeVector      saveFile (edict^.eAVelocity)
    
    writeInt         saveFile (edict^.eMass)
    writeFloat       saveFile (edict^.eAirFinished)
    
    writeFloat       saveFile (edict^.eGravity)
    
    writeEdictRef    saveFile (edict^.eGoalEntity)
    writeEdictRef    saveFile (edict^.eMoveTarget)
    
    writeFloat       saveFile (edict^.eYawSpeed)
    writeFloat       saveFile (edict^.eIdealYaw)
    
    writeFloat       saveFile (edict^.eNextThink)
    
    writeAdapter     saveFile (edict^.ePrethink)
    writeAdapter     saveFile (edict^.eThink)
    writeAdapter     saveFile (edict^.eBlocked)
    
    writeAdapter     saveFile (edict^.eTouch)
    writeAdapter     saveFile (edict^.eUse)
    writeAdapter     saveFile (edict^.ePain)
    writeAdapter     saveFile (edict^.eDie)
    
    writeFloat       saveFile (edict^.eTouchDebounceTime)
    writeFloat       saveFile (edict^.ePainDebounceTime)
    writeFloat       saveFile (edict^.eDamageDebounceTime)
    
    writeFloat       saveFile (edict^.eFlySoundDebounceTime)
    writeFloat       saveFile (edict^.eLastMoveTime)
    
    writeInt         saveFile (edict^.eHealth)
    writeInt         saveFile (edict^.eMaxHealth)
    
    writeInt         saveFile (edict^.eGibHealth)
    writeInt         saveFile (edict^.eDeadFlag)
    writeInt         saveFile (edict^.eShowHostile)
    
    writeFloat       saveFile (edict^.ePowerArmorTime)
    
    writeString      saveFile (edict^.eMap)
    
    writeInt         saveFile (edict^.eViewHeight)
    writeInt         saveFile (edict^.eTakeDamage)
    writeInt         saveFile (edict^.eDmg)
    writeInt         saveFile (edict^.eRadiusDmg)
    writeFloat       saveFile (edict^.eDmgRadius)
    
    writeInt         saveFile (edict^.eSounds)
    writeInt         saveFile (edict^.eCount)
    
    writeEdictRef    saveFile (edict^.eChain)
    writeEdictRef    saveFile (edict^.eEnemy)
    writeEdictRef    saveFile (edict^.eOldEnemy)
    writeEdictRef    saveFile (edict^.eActivator)
    writeEdictRef    saveFile (edict^.eGroundEntity)
    writeInt         saveFile (edict^.eGroundEntityLinkCount)
    writeEdictRef    saveFile (edict^.eTeamChain)
    writeEdictRef    saveFile (edict^.eTeamMaster)
    
    writeEdictRef    saveFile (edict^.eMyNoise)
    writeEdictRef    saveFile (edict^.eMyNoise2)
    
    writeInt         saveFile (edict^.eNoiseIndex)
    writeInt         saveFile (edict^.eNoiseIndex2)
    
    writeFloat       saveFile (edict^.eVolume)
    writeFloat       saveFile (edict^.eAttenuation)
    writeFloat       saveFile (edict^.eWait)
    writeFloat       saveFile (edict^.eDelay)
    writeFloat       saveFile (edict^.eRandom)
    
    writeFloat       saveFile (edict^.eTeleportTime)
    
    writeInt         saveFile (edict^.eWaterType)
    writeInt         saveFile (edict^.eWaterLevel)
    writeVector      saveFile (edict^.eMoveOrigin)
    writeVector      saveFile (edict^.eMoveAngles)
    
    writeInt         saveFile (edict^.eLightLevel)
    writeInt         saveFile (edict^.eStyle)
    
    writeItemRef     saveFile (edict^.eItem)
    
    writeMoveInfo    saveFile (edict^.eMoveInfo)
    writeMonsterInfo saveFile (edict^.eMonsterInfo)
    
    case edict^.eClient of
      Nothing -> writeInt saveFile (-1)
      Just (Ref idx) -> writeInt saveFile idx
      
    writeEdictRef    saveFile (edict^.eOwner)
    
    -- rst's checker :-)
    writeInt         saveFile 9876

writeEntityState :: QuakeFile -> EntityStateT -> IO ()
writeEntityState saveFile entityState = do
    print "writeEntityState"
    writeEdictRef saveFile (entityState^.esSurroundingEnt)
    writeVector   saveFile (entityState^.esOrigin)
    writeVector   saveFile (entityState^.esAngles)
    writeVector   saveFile (entityState^.esOldOrigin)
    
    writeInt      saveFile (entityState^.esModelIndex)
    
    writeInt      saveFile (entityState^.esModelIndex2)
    writeInt      saveFile (entityState^.esModelIndex3)
    writeInt      saveFile (entityState^.esModelIndex4)
    
    writeInt      saveFile (entityState^.esFrame)
    writeInt      saveFile (entityState^.esSkinNum)
    
    writeInt      saveFile (entityState^.esEffects)
    writeInt      saveFile (entityState^.esRenderFx)
    writeInt      saveFile (entityState^.esSolid)
    
    writeInt      saveFile (entityState^.esSound)
    writeInt      saveFile (entityState^.esEvent)

writeAdapter :: SuperAdapter a => QuakeFile -> Maybe a -> IO ()
writeAdapter saveFile mAdapter = do
    print "writeAdapter"
    writeInt saveFile 3988
    
    case mAdapter of
      Nothing -> writeString saveFile Nothing
      Just adapter -> writeString saveFile (Just $ getID adapter)

writeItemRef :: QuakeFile -> Maybe GItemReference -> IO ()
writeItemRef saveFile Nothing = writeInt saveFile (-1)
writeItemRef saveFile (Just (GItemReference idx)) = writeInt saveFile idx

readItemRef :: QuakeFile -> IO (Maybe GItemReference)
readItemRef saveFile = do
    idx <- readInt saveFile

    return $ if idx == -1
               then Nothing
               else Just (GItemReference idx)

writeMoveInfo :: QuakeFile -> MoveInfoT -> IO ()
writeMoveInfo saveFile moveInfo = do
    print "writeMoveInfo"
    writeVector saveFile  (moveInfo^.miStartOrigin)
    writeVector saveFile  (moveInfo^.miStartAngles)
    writeVector saveFile  (moveInfo^.miEndOrigin)
    writeVector saveFile  (moveInfo^.miEndAngles)
    
    writeInt    saveFile  (moveInfo^.miSoundStart)
    writeInt    saveFile  (moveInfo^.miSoundMiddle)
    writeInt    saveFile  (moveInfo^.miSoundEnd)
    
    writeFloat  saveFile  (moveInfo^.miAccel)
    writeFloat  saveFile  (moveInfo^.miSpeed)
    writeFloat  saveFile  (moveInfo^.miDecel)
    writeFloat  saveFile  (moveInfo^.miDistance)
    
    writeFloat  saveFile  (moveInfo^.miWait)
    
    writeInt    saveFile  (moveInfo^.miState)
    writeVector saveFile  (moveInfo^.miDir)
    
    writeFloat  saveFile  (moveInfo^.miCurrentSpeed)
    writeFloat  saveFile  (moveInfo^.miMoveSpeed)
    writeFloat  saveFile  (moveInfo^.miNextSpeed)
    writeFloat  saveFile  (moveInfo^.miRemainingDistance)
    writeFloat  saveFile  (moveInfo^.miDecelDistance)
    writeAdapter saveFile (moveInfo^.miEndFunc)

writeMonsterInfo :: QuakeFile -> MonsterInfoT -> IO ()
writeMonsterInfo saveFile monsterInfo = do
    print "writeMonsterInfo"
    case monsterInfo^.miCurrentMove of
      Nothing -> writeBool saveFile False
      Just move -> writeBool saveFile True >> writeMMove saveFile move
    
    writeInt     saveFile (monsterInfo^.miAIFlags)
    writeInt     saveFile (monsterInfo^.miNextFrame)
    writeFloat   saveFile (monsterInfo^.miScale)
    writeAdapter saveFile (monsterInfo^.miStand)
    writeAdapter saveFile (monsterInfo^.miIdle)
    writeAdapter saveFile (monsterInfo^.miSearch)
    writeAdapter saveFile (monsterInfo^.miWalk)
    writeAdapter saveFile (monsterInfo^.miRun)
    
    writeAdapter saveFile (monsterInfo^.miDodge)
    
    writeAdapter saveFile (monsterInfo^.miAttack)
    writeAdapter saveFile (monsterInfo^.miMelee)
    
    writeAdapter saveFile (monsterInfo^.miSight)
    
    writeAdapter saveFile (monsterInfo^.miCheckAttack)
    
    writeFloat   saveFile (monsterInfo^.miPauseTime)
    writeFloat   saveFile (monsterInfo^.miAttackFinished)
    
    writeVector  saveFile (monsterInfo^.miSavedGoal)
    
    writeFloat   saveFile (monsterInfo^.miSearchTime)
    writeFloat   saveFile (monsterInfo^.miTrailTime)
    
    writeVector  saveFile (monsterInfo^.miLastSighting)
    
    writeInt     saveFile (monsterInfo^.miAttackState)
    writeInt     saveFile (monsterInfo^.miLefty)
    
    writeFloat   saveFile (monsterInfo^.miIdleTime)
    writeInt     saveFile (monsterInfo^.miLinkCount)
    
    writeInt     saveFile (monsterInfo^.miPowerArmorPower)
    writeInt     saveFile (monsterInfo^.miPowerArmorType)

writeMMove :: QuakeFile -> MMoveT -> IO ()
writeMMove saveFile move = do
    print "writeMMove"
    writeString  saveFile (Just $ move^.mmId)
    writeInt     saveFile (move^.mmFirstFrame)
    writeInt     saveFile (move^.mmLastFrame)
    writeInt     saveFile (V.length (move^.mmFrame))
    V.mapM_ (writeMFrame saveFile) (move^.mmFrame)
    writeAdapter saveFile (move^.mmEndFunc)

writeMFrame :: QuakeFile -> MFrameT -> IO ()
writeMFrame saveFile frame = do
    print "writeMFrame"
    writeAdapter saveFile (frame^.mfAI)
    writeFloat   saveFile (frame^.mfDist)
    writeAdapter saveFile (frame^.mfThink)

writeGClient :: QuakeFile -> GClientT -> IO ()
writeGClient saveFile gClient = do
    print "writeGClient"
    writePlayerState      saveFile (gClient^.gcPlayerState)

    writeInt              saveFile (gClient^.gcPing)

    writeClientPersistant saveFile (gClient^.gcPers)
    writeClientRespawn    saveFile (gClient^.gcResp)

    writePMoveState       saveFile (gClient^.gcOldPMove)

    writeBool             saveFile (gClient^.gcShowScores)
    writeBool             saveFile (gClient^.gcShowInventory)
    writeBool             saveFile (gClient^.gcShowHelp)
    writeBool             saveFile (gClient^.gcShowHelpIcon)
    writeInt              saveFile (gClient^.gcAmmoIndex)

    writeInt              saveFile (gClient^.gcButtons)
    writeInt              saveFile (gClient^.gcOldButtons)
    writeInt              saveFile (gClient^.gcLatchedButtons)

    writeBool             saveFile (gClient^.gcWeaponThunk)
    writeItemRef          saveFile (gClient^.gcNewWeapon)

    writeInt              saveFile (gClient^.gcDamageArmor)
    writeInt              saveFile (gClient^.gcDamagePArmor)
    writeInt              saveFile (gClient^.gcDamageBlood)
    writeInt              saveFile (gClient^.gcDamageKnockback)

    writeVector           saveFile (gClient^.gcDamageFrom)

    writeFloat            saveFile (gClient^.gcKillerYaw)

    writeInt              saveFile (gClient^.gcWeaponState)

    writeVector           saveFile (gClient^.gcKickAngles)
    writeVector           saveFile (gClient^.gcKickOrigin)

    writeFloat            saveFile (gClient^.gcVDmgRoll)
    writeFloat            saveFile (gClient^.gcVDmgPitch)
    writeFloat            saveFile (gClient^.gcVDmgTime)
    writeFloat            saveFile (gClient^.gcFallTime)
    writeFloat            saveFile (gClient^.gcFallValue)
    writeFloat            saveFile (gClient^.gcDamageAlpha)
    writeFloat            saveFile (gClient^.gcBonusAlpha)

    writeVector           saveFile (gClient^.gcDamageBlend)
    writeVector           saveFile (gClient^.gcVAngle)

    writeFloat            saveFile (gClient^.gcBobTime)

    writeVector           saveFile (gClient^.gcOldViewAngles)
    writeVector           saveFile (gClient^.gcOldVelocity)

    writeFloat            saveFile (gClient^.gcNextDrownTime)

    writeInt              saveFile (gClient^.gcOldWaterLevel)
    writeInt              saveFile (gClient^.gcBreatherSound)
    writeInt              saveFile (gClient^.gcMachinegunShots)
    writeInt              saveFile (gClient^.gcAnimEnd)
    writeInt              saveFile (gClient^.gcAnimPriority)
    writeBool             saveFile (gClient^.gcAnimDuck)
    writeBool             saveFile (gClient^.gcAnimRun)

    writeFloat            saveFile (gClient^.gcQuadFrameNum)
    writeFloat            saveFile (gClient^.gcInvincibleFrameNum)
    writeFloat            saveFile (gClient^.gcBreatherFrameNum)
    writeFloat            saveFile (gClient^.gcEnviroFrameNum)

    writeBool             saveFile (gClient^.gcGrenadeBlewUp)
    writeFloat            saveFile (gClient^.gcGrenadeTime)
    writeInt              saveFile (gClient^.gcSilencerShots)
    writeInt              saveFile (gClient^.gcWeaponSound)
    writeFloat            saveFile (gClient^.gcPickupMsgTime)
    writeFloat            saveFile (gClient^.gcFloodLockTill)
    UV.mapM_ (writeFloat saveFile) (gClient^.gcFloodWhen)
    writeInt              saveFile (gClient^.gcFloodWhenHead)
    writeFloat            saveFile (gClient^.gcRespawnTime)
    writeEdictRef         saveFile (gClient^.gcChaseTarget)
    writeBool             saveFile (gClient^.gcUpdateChase)

    writeInt              saveFile 8765

readGClient :: QuakeFile -> Int -> IO (GClientT)
readGClient saveFile idx = do
    print "readGClient"
    playerState <- readPlayerState saveFile

    ping <- readInt saveFile

    pers <- readClientPersistant saveFile
    resp <- readClientRespawn saveFile

    oldPMove <- readPMoveState saveFile

    showScores <- readBool saveFile
    showInventory <- readBool saveFile
    showHelp <- readBool saveFile
    showHelpIcon <- readBool saveFile
    ammoIndex <- readInt saveFile

    buttons <- readInt saveFile
    oldButtons <- readInt saveFile
    latchedButtons <- readInt saveFile

    weaponThunk <- readBool saveFile

    newWeapon <- readItemRef saveFile

    damageArmor <- readInt saveFile
    damagePArmor <- readInt saveFile
    damageBlood <- readInt saveFile
    damageKnockback <- readInt saveFile

    damageFrom <- readVector saveFile
    killerYaw <- readFloat saveFile
    weaponState <- readInt saveFile

    kickAngles <- readVector saveFile
    kickOrigin <- readVector saveFile

    vDmgRoll <- readFloat saveFile
    vDmgPitch <- readFloat saveFile
    vDmgTime <- readFloat saveFile
    fallTime <- readFloat saveFile
    fallValue <- readFloat saveFile
    damageAlpha <- readFloat saveFile
    bonusAlpha <- readFloat saveFile

    damageBlend <- readVector saveFile
    vAngle <- readVector saveFile

    bobTime <- readFloat saveFile

    oldViewAngles <- readVector saveFile
    oldVelocity <- readVector saveFile

    nextDrownTime <- readFloat saveFile

    oldWaterLevel <- readInt saveFile
    breatherSound <- readInt saveFile
    machinegunShots <- readInt saveFile
    animEnd <- readInt saveFile
    animPriority <- readInt saveFile
    animDuck <- readBool saveFile
    animRun <- readBool saveFile

    quadFrameNum <- readFloat saveFile
    invincibleFrameNum <- readFloat saveFile
    breatherFrameNum <- readFloat saveFile
    enviroFrameNum <- readFloat saveFile

    grenadeBlewUp <- readBool saveFile
    grenadeTime <- readFloat saveFile
    silencerShots <- readInt saveFile
    weaponSound <- readInt saveFile
    pickupMsgTime <- readFloat saveFile
    floodLockTill <- readFloat saveFile

    floodWhen <- mapM (const $ readFloat saveFile) [0..9]

    floodWhenHead <- readInt saveFile
    respawnTime <- readFloat saveFile
    chaseTarget <- readEdictRef saveFile
    updateChase <- readBool saveFile

    check <- readInt saveFile

    return GClientT { _gcPlayerState        = playerState
                    , _gcPing               = ping
                    , _gcPers               = pers
                    , _gcResp               = resp
                    , _gcOldPMove           = oldPMove
                    , _gcShowScores         = showScores
                    , _gcShowInventory      = showInventory
                    , _gcShowHelp           = showHelp
                    , _gcShowHelpIcon       = showHelpIcon
                    , _gcAmmoIndex          = ammoIndex
                    , _gcButtons            = buttons
                    , _gcOldButtons         = oldButtons
                    , _gcLatchedButtons     = latchedButtons
                    , _gcWeaponThunk        = weaponThunk
                    , _gcNewWeapon          = newWeapon
                    , _gcDamageArmor        = damageArmor
                    , _gcDamagePArmor       = damagePArmor
                    , _gcDamageBlood        = damageBlood
                    , _gcDamageKnockback    = damageKnockback
                    , _gcDamageFrom         = damageFrom
                    , _gcKillerYaw          = killerYaw
                    , _gcWeaponState        = weaponState
                    , _gcKickAngles         = kickAngles
                    , _gcKickOrigin         = kickOrigin
                    , _gcVDmgRoll           = vDmgRoll
                    , _gcVDmgPitch          = vDmgPitch
                    , _gcVDmgTime           = vDmgTime
                    , _gcFallTime           = fallTime
                    , _gcFallValue          = fallValue
                    , _gcDamageAlpha        = damageAlpha
                    , _gcBonusAlpha         = bonusAlpha
                    , _gcDamageBlend        = damageBlend
                    , _gcVAngle             = vAngle
                    , _gcBobTime            = bobTime
                    , _gcOldViewAngles      = oldViewAngles
                    , _gcOldVelocity        = oldVelocity
                    , _gcNextDrownTime      = nextDrownTime
                    , _gcOldWaterLevel      = oldWaterLevel
                    , _gcBreatherSound      = breatherSound
                    , _gcMachinegunShots    = machinegunShots
                    , _gcAnimEnd            = animEnd
                    , _gcAnimPriority       = animPriority
                    , _gcAnimDuck           = animDuck
                    , _gcAnimRun            = animRun
                    , _gcQuadFrameNum       = quadFrameNum
                    , _gcInvincibleFrameNum = invincibleFrameNum
                    , _gcBreatherFrameNum   = breatherFrameNum
                    , _gcEnviroFrameNum     = enviroFrameNum
                    , _gcGrenadeBlewUp      = grenadeBlewUp
                    , _gcGrenadeTime        = grenadeTime
                    , _gcSilencerShots      = silencerShots
                    , _gcWeaponSound        = weaponSound
                    , _gcPickupMsgTime      = pickupMsgTime
                    , _gcFloodLockTill      = floodLockTill
                    , _gcFloodWhen          = UV.fromList floodWhen
                    , _gcFloodWhenHead      = floodWhenHead
                    , _gcRespawnTime        = respawnTime
                    , _gcChaseTarget        = chaseTarget
                    , _gcUpdateChase        = updateChase
                    , _gcIndex              = idx
                    }

{- TODO: how do we do it?
    when (check /= 8765) $
      Com.dprintf "game client load failed for num=" + index
      -}

writeGameLocals :: QuakeFile -> GameLocalsT -> IO ()
writeGameLocals saveFile gameLocals = do
    print "writeGameLocals"
    writeString saveFile (Just $ gameLocals^.glHelpMessage1)
    writeString saveFile (Just $ gameLocals^.glHelpMessage2)

    writeInt    saveFile (gameLocals^.glHelpChanged)

    writeString saveFile (Just $ gameLocals^.glSpawnPoint)
    writeInt    saveFile (gameLocals^.glMaxClients)
    writeInt    saveFile (gameLocals^.glMaxEntities)
    writeInt    saveFile (gameLocals^.glServerFlags)
    writeInt    saveFile (gameLocals^.glNumItems)
    writeBool   saveFile (gameLocals^.glAutosaved)
    -- rst's checker :-)
    writeInt    saveFile 1928

readGameLocals :: QuakeFile -> IO (GameLocalsT)
readGameLocals saveFile = do
    print "readGameLocals"
    Just helpMessage1 <- readString saveFile
    Just helpMessage2 <- readString saveFile

    helpChanged <- readInt saveFile

    Just spawnPoint <- readString saveFile
    maxClients <- readInt saveFile
    maxEntities <- readInt saveFile
    serverFlags <- readInt saveFile
    numItems <- readInt saveFile
    autoSaved <- readBool saveFile

    check <- readInt saveFile

    return newGameLocalsT { _glHelpMessage1 = helpMessage1
                          , _glHelpMessage2 = helpMessage2
                          , _glHelpChanged  = helpChanged
                          , _glSpawnPoint   = spawnPoint
                          , _glMaxClients   = maxClients
                          , _glMaxEntities  = maxEntities
                          , _glServerFlags  = serverFlags
                          , _glNumItems     = numItems
                          , _glAutosaved    = autoSaved
                          }
    
{- TODO: how do we do it?
    when (check /= 1928) $
      Com.dprintf "error in loading game_locals, 1928\n"
      -}

writePlayerState :: QuakeFile -> PlayerStateT -> IO ()
writePlayerState saveFile playerState = do
    print "writePlayerState"
    writePMoveState saveFile (playerState^.psPMoveState)

    writeVector     saveFile (playerState^.psViewAngles)
    writeVector     saveFile (playerState^.psViewOffset)
    writeVector     saveFile (playerState^.psKickAngles)
    writeVector     saveFile (playerState^.psGunAngles)
    writeVector     saveFile (playerState^.psGunOffset)

    writeInt        saveFile (playerState^.psGunIndex)
    writeInt        saveFile (playerState^.psGunFrame)

    let V4 a b c d = playerState^.psBlend
    writeFloat      saveFile a
    writeFloat      saveFile b
    writeFloat      saveFile c
    writeFloat      saveFile d

    writeFloat      saveFile (playerState^.psFOV)

    writeInt        saveFile (playerState^.psRDFlags)

    UV.mapM_ (writeShort saveFile) (playerState^.psStats)

readPlayerState :: QuakeFile -> IO (PlayerStateT)
readPlayerState saveFile = do
    print "readPlayerState"
    pMoveState <- readPMoveState saveFile

    viewAngles <- readVector saveFile
    viewOffset <- readVector saveFile
    kickAngles <- readVector saveFile
    gunAngles <- readVector saveFile
    gunOffset <- readVector saveFile

    gunIndex <- readInt saveFile
    gunFrame <- readInt saveFile

    a <- readFloat saveFile
    b <- readFloat saveFile
    c <- readFloat saveFile
    d <- readFloat saveFile

    fov <- readFloat saveFile
    rdFlags <- readInt saveFile
    
    stats <- mapM (const $ readShort saveFile) [0..Constants.maxStats-1]

    return PlayerStateT { _psPMoveState = pMoveState
                        , _psViewAngles = viewAngles
                        , _psViewOffset = viewOffset
                        , _psKickAngles = kickAngles
                        , _psGunAngles  = gunAngles
                        , _psGunOffset  = gunOffset
                        , _psGunIndex   = gunIndex
                        , _psGunFrame   = gunFrame
                        , _psBlend      = V4 a b c d
                        , _psFOV        = fov
                        , _psRDFlags    = rdFlags
                        , _psStats      = UV.fromList stats
                        }

writeClientPersistant :: QuakeFile -> ClientPersistantT -> IO ()
writeClientPersistant saveFile clientPersistant = do
    print "writeClientPersistant"
    writeString saveFile (Just $ clientPersistant^.cpUserInfo)
    writeString saveFile (Just $ clientPersistant^.cpNetName)

    writeInt    saveFile (clientPersistant^.cpHand)

    writeBool   saveFile (clientPersistant^.cpConnected)
    writeInt    saveFile (clientPersistant^.cpHealth)

    writeInt    saveFile (clientPersistant^.cpMaxHealth)
    writeInt    saveFile (clientPersistant^.cpSavedFlags)
    writeInt    saveFile (clientPersistant^.cpSelectedItem)

    UV.mapM_ (writeInt saveFile) (clientPersistant^.cpInventory)

    writeInt    saveFile (clientPersistant^.cpMaxBullets)
    writeInt    saveFile (clientPersistant^.cpMaxShells)
    writeInt    saveFile (clientPersistant^.cpMaxRockets)
    writeInt    saveFile (clientPersistant^.cpMaxGrenades)
    writeInt    saveFile (clientPersistant^.cpMaxCells)
    writeInt    saveFile (clientPersistant^.cpMaxSlugs)

    writeItemRef saveFile (clientPersistant^.cpWeapon)
    writeItemRef saveFile (clientPersistant^.cpLastWeapon)
    writeInt     saveFile (clientPersistant^.cpPowerCubes)
    writeInt     saveFile (clientPersistant^.cpScore)

    writeInt     saveFile (clientPersistant^.cpGameHelpChanged)
    writeInt     saveFile (clientPersistant^.cpHelpChanged)
    writeBool    saveFile (clientPersistant^.cpSpectator)

readClientPersistant :: QuakeFile -> IO (ClientPersistantT)
readClientPersistant saveFile = do
    print "readClientPersistant"
    Just userInfo <- readString saveFile
    Just netName <- readString saveFile

    hand <- readInt saveFile

    connected <- readBool saveFile
    health <- readInt saveFile

    maxHealth <- readInt saveFile
    savedFlags <- readInt saveFile
    selectedItem <- readInt saveFile

    inventory <- mapM (const $ readInt saveFile) [0..Constants.maxItems-1]

    maxBullets <- readInt saveFile
    maxShells <- readInt saveFile
    maxRockets <- readInt saveFile
    maxGrenades <- readInt saveFile
    maxCells <- readInt saveFile
    maxSlugs <- readInt saveFile

    weapon <- readItemRef saveFile
    lastWeapon <- readItemRef saveFile
    powerCubes <- readInt saveFile
    score <- readInt saveFile

    gameHelpChanged <- readInt saveFile
    helpChanged <- readInt saveFile
    spectator <- readBool saveFile

    return ClientPersistantT { _cpUserInfo        = userInfo
                             , _cpNetName         = netName
                             , _cpHand            = hand
                             , _cpConnected       = connected
                             , _cpHealth          = health
                             , _cpMaxHealth       = maxHealth
                             , _cpSavedFlags      = savedFlags
                             , _cpSelectedItem    = selectedItem
                             , _cpInventory       = UV.fromList inventory
                             , _cpMaxBullets      = maxBullets
                             , _cpMaxShells       = maxShells
                             , _cpMaxRockets      = maxRockets
                             , _cpMaxGrenades     = maxGrenades
                             , _cpMaxCells        = maxCells
                             , _cpMaxSlugs        = maxSlugs
                             , _cpWeapon          = weapon
                             , _cpLastWeapon      = lastWeapon
                             , _cpPowerCubes      = powerCubes
                             , _cpScore           = score
                             , _cpGameHelpChanged = gameHelpChanged
                             , _cpHelpChanged     = helpChanged
                             , _cpSpectator       = spectator
                             }

writeClientRespawn :: QuakeFile -> ClientRespawnT -> IO ()
writeClientRespawn saveFile clientRespawn = do
    print "writeClientRespawn"
    writeClientPersistant saveFile (clientRespawn^.crCoopRespawn)
    writeInt              saveFile (clientRespawn^.crEnterFrame)
    writeInt              saveFile (clientRespawn^.crScore)
    writeVector           saveFile (clientRespawn^.crCmdAngles)
    writeBool             saveFile (clientRespawn^.crSpectator)

readClientRespawn :: QuakeFile -> IO (ClientRespawnT)
readClientRespawn saveFile = do
    print "readClientRespawn"
    coopRespawn <- readClientPersistant saveFile
    enterFrame <- readInt saveFile
    score <- readInt saveFile
    cmdAngles <- readVector saveFile
    spectator <- readBool saveFile

    return ClientRespawnT { _crCoopRespawn = coopRespawn
                          , _crEnterFrame  = enterFrame
                          , _crScore       = score
                          , _crCmdAngles   = cmdAngles
                          , _crSpectator   = spectator
                          }

writePMoveState :: QuakeFile -> PMoveStateT -> IO ()
writePMoveState saveFile pMoveState = do
    print "writePMoveState"
    writeInt saveFile (pMoveState^.pmsPMType)

    writeVectorShort saveFile (pMoveState^.pmsOrigin)
    writeVectorShort saveFile (pMoveState^.pmsVelocity)

    writeByte        saveFile (pMoveState^.pmsPMFlags)
    writeByte        saveFile (pMoveState^.pmsPMTime)
    writeShort       saveFile (pMoveState^.pmsGravity)

    writeShort       saveFile 0

    writeVectorShort saveFile (pMoveState^.pmsDeltaAngles)

readPMoveState :: QuakeFile -> IO (PMoveStateT)
readPMoveState saveFile = do
    print "readPMoveState"
    pmType <- readInt saveFile
    origin <- readVectorShort saveFile
    velocity <- readVectorShort saveFile
    pmFlags <- readByte saveFile
    pmTime <- readByte saveFile
    gravity <- readShort saveFile

    void $ readShort saveFile

    deltaAngles <- readVectorShort saveFile

    return PMoveStateT { _pmsPMType      = pmType
                       , _pmsOrigin      = origin
                       , _pmsVelocity    = velocity
                       , _pmsPMFlags     = pmFlags
                       , _pmsPMTime      = pmTime
                       , _pmsGravity     = gravity
                       , _pmsDeltaAngles = deltaAngles
                       }
