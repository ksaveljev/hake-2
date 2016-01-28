{-# LANGUAGE ScopedTypeVariables #-}
module Util.QuakeFile ( QuakeFile
                      , open
                      , close
                      , writeString
                      , readString
                      , writeInt
                      , readInt
                      , writeEdict
                      , writeLevelLocals
                      , writeGClient
                      , writeGameLocals
                      ) where

import Control.Lens ((^.))
import Data.Functor ((<$>))
import Data.Binary.Get
import Data.Binary.Put
import Linear (V3(..))
import System.IO (openFile, IOMode(ReadWriteMode), Handle, hClose)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import Internal
import Quake
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
writeString (QuakeFile h) Nothing = BL.hPut h $ runPut $ putInt (-1)
writeString (QuakeFile h) (Just str) =
    BL.hPut h $ runPut $ do
      putInt (B.length str)
      putByteString str

readString :: QuakeFile -> IO B.ByteString
readString (QuakeFile h) = do
    stringSize <- BL.hGet h 4
    let len :: Int = runGet getInt stringSize
    B.hGet h len

writeInt :: QuakeFile -> Int -> IO ()
writeInt (QuakeFile h) num =
    BL.hPut h $ runPut $ putInt num

readInt :: QuakeFile -> IO Int
readInt (QuakeFile h) = do
    num <- BL.hGet h 4
    return (runGet getInt num)

writeFloat :: QuakeFile -> Float -> IO ()
writeFloat (QuakeFile h) num =
    BL.hPut h $ runPut $ putFloat num
    
readFloat :: QuakeFile -> IO Float
readFloat (QuakeFile h) = do
    num <- BL.hGet h 4
    return (runGet getFloat num)

writeBool :: QuakeFile -> Bool -> IO ()
writeBool (QuakeFile h) p =
    BL.hPut h $ runPut $ putBool p

readBool :: QuakeFile -> IO Bool
readBool (QuakeFile h) = do
    p <- BL.hGet h 1
    return (runGet getBool p)

writeEdictRef :: QuakeFile -> Maybe EdictReference -> IO ()
writeEdictRef saveFile Nothing = writeInt saveFile (-1)
writeEdictRef saveFile (Just (EdictReference idx)) = writeInt saveFile idx

readEdictRef :: QuakeFile -> IO (Maybe EdictReference)
readEdictRef (QuakeFile h) = do
    num <- BL.hGet h 4
    let idx = runGet getInt num
    
    return $ if idx == -1
               then Nothing
               else Just (EdictReference idx) -- IMPROVE: if (i > GameBase.g_edicts.length) {
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

writeLevelLocals :: QuakeFile -> LevelLocalsT -> IO ()
writeLevelLocals saveFile level = do
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

writeEdict :: QuakeFile -> EdictT -> IO ()
writeEdict saveFile edict = do
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
      Just (GClientReference idx) -> writeInt saveFile idx
      
    writeEdictRef    saveFile (edict^.eOwner)
    
    -- rst's checker :-)
    writeInt         saveFile 9876

writeEntityState :: QuakeFile -> EntityStateT -> IO ()
writeEntityState saveFile entityState = do
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
    writeInt saveFile 3988
    
    case mAdapter of
      Nothing -> writeString saveFile Nothing
      Just adapter -> writeString saveFile (Just $ getID adapter)

writeItemRef :: QuakeFile -> Maybe GItemReference -> IO ()
writeItemRef saveFile Nothing = writeInt saveFile (-1)
writeItemRef saveFile (Just (GItemReference idx)) = writeInt saveFile idx

writeMoveInfo :: QuakeFile -> MoveInfoT -> IO ()
writeMoveInfo saveFile moveInfo = do
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
    writeString  saveFile (Just $ move^.mmId)
    writeInt     saveFile (move^.mmFirstFrame)
    writeInt     saveFile (move^.mmLastFrame)
    writeInt     saveFile (V.length (move^.mmFrame))
    V.mapM_ (writeMFrame saveFile) (move^.mmFrame)
    writeAdapter saveFile (move^.mmEndFunc)

writeMFrame :: QuakeFile -> MFrameT -> IO ()
writeMFrame saveFile frame = do
    writeAdapter saveFile (frame^.mfAI)
    writeFloat   saveFile (frame^.mfDist)
    writeAdapter saveFile (frame^.mfThink)

writeGClient :: QuakeFile -> GClientT -> IO ()
writeGClient _ _ = do
    io (putStrLn "QuakeFile.writeGClient") >> undefined -- TODO

writeGameLocals :: QuakeFile -> GameLocalsT -> IO ()
writeGameLocals saveFile gameLocals = do
    -- f.writeString(new Date().toString()); -- TODO: what for ?

    writeString saveFile (Just $ gameLocals^.glHelpMessage1)
    writeString saveFile (Just $ gameLocals^.glHelpMessage2)

    writeInt    saveFile (gameLocals^.glHelpChanged)

    writeString saveFile (Just $ gameLocals^.glSpawnPoint)
    writeInt    saveFile (gameLocals^.glMaxClients)
    writeInt    saveFile (gameLocals^.glMaxEntities)
    writeInt    saveFile (gameLocals^.glServerFlags)
    writeInt    saveFile (gameLocals^.glNumItems)
    writeInt    saveFile (if gameLocals^.glAutosaved then 1 else 0)
    -- rst's checker :-)
    writeInt    saveFile 1928
