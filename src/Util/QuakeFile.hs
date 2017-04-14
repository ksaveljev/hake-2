module Util.QuakeFile
    ( close
    , open
    , readString
    , writeBool
    , writeGameLocals
    , writeGClient
    , writeInt
    , writeString
    ) where

import           Control.Lens           ((^.))
import           Data.Binary.Put        (runPut, putByteString, putWord8)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Lazy   as BL
import           Data.Foldable          (traverse_)
import           Data.Int               (Int8, Int16)
import qualified Data.Vector.Unboxed    as UV
import           Linear                 (V3(..), V4(..))
import           System.IO              (IOMode(ReadWriteMode))

import           Game.ClientPersistantT
import           Game.ClientRespawnT
import           Game.GameLocalsT
import           Game.GClientT
import           Game.PlayerStateT
import           Game.PMoveStateT
import           Types
import           Util.Binary            (putInt, putInt16, putBool, putFloat)
import qualified Util.Lib               as Lib

open :: B.ByteString -> Quake (Maybe QuakeFile)
open name = (QuakeFile <$>) <$> Lib.fOpen name ReadWriteMode

close :: QuakeFile -> Quake ()
close (QuakeFile h) = Lib.fClose h

readString :: QuakeFile -> IO (Maybe B.ByteString)
readString = error "QuakeFile.readString" -- TODO

writeBool :: QuakeFile -> Bool -> IO ()
writeBool (QuakeFile h) p = BL.hPut h $ runPut $ putBool p

writeByte :: QuakeFile -> Int8 -> IO ()
writeByte (QuakeFile h) num = BL.hPut h $ runPut $ putWord8 (fromIntegral num)

writeShort :: QuakeFile -> Int16 -> IO ()
writeShort (QuakeFile h) num = BL.hPut h $ runPut $ putInt16 num

writeFloat :: QuakeFile -> Float -> IO ()
writeFloat (QuakeFile h) num = BL.hPut h $ runPut $ putFloat num

writeInt :: QuakeFile -> Int -> IO ()
writeInt (QuakeFile h) num = BL.hPut h $ runPut $ putInt num

writeVector :: QuakeFile -> V3 Float -> IO ()
writeVector saveFile (V3 a b c) = do
    writeFloat saveFile a
    writeFloat saveFile b
    writeFloat saveFile c

writeVectorShort :: QuakeFile -> V3 Int16 -> IO ()
writeVectorShort saveFile (V3 a b c) = do
    writeShort saveFile a
    writeShort saveFile b
    writeShort saveFile c

writeString :: QuakeFile -> Maybe B.ByteString -> IO ()
writeString (QuakeFile h) Nothing = do
    BL.hPut h (runPut (putInt (-1)))
writeString (QuakeFile h) (Just str) = do
    BL.hPut h $ runPut $ do
        putInt (B.length str)
        putByteString str

writeEdictRef :: QuakeFile -> Maybe (Ref EdictT) -> IO ()
writeEdictRef saveFile Nothing = writeInt saveFile (-1)
writeEdictRef saveFile (Just (Ref idx)) = writeInt saveFile idx

writeItemRef :: QuakeFile -> Maybe (Ref GItemT) -> IO ()
writeItemRef saveFile Nothing = writeInt saveFile (-1)
writeItemRef saveFile (Just (Ref idx)) = writeInt saveFile idx

writeGameLocals :: QuakeFile -> GameLocalsT -> IO ()
writeGameLocals saveFile gameLocals = do
    writeString saveFile (Just (gameLocals^.glHelpMessage1))
    writeString saveFile (Just (gameLocals^.glHelpMessage2))
    writeInt    saveFile (gameLocals^.glHelpChanged)
    writeString saveFile (Just (gameLocals^.glSpawnPoint))
    writeInt    saveFile (gameLocals^.glMaxClients)
    writeInt    saveFile (gameLocals^.glMaxEntities)
    writeInt    saveFile (gameLocals^.glServerFlags)
    writeInt    saveFile (gameLocals^.glNumItems)
    writeBool   saveFile (gameLocals^.glAutosaved)
    -- rst's checker :-)
    writeInt    saveFile 1928

writeGClient :: QuakeFile -> GClientT -> IO ()
writeGClient saveFile gClient = do
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

writePlayerState :: QuakeFile -> PlayerStateT -> IO ()
writePlayerState saveFile playerState = do
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

writeClientPersistant :: QuakeFile -> ClientPersistantT -> IO ()
writeClientPersistant saveFile clientPersistant = do
    writeString saveFile (Just (clientPersistant^.cpUserInfo))
    writeString saveFile (Just (clientPersistant^.cpNetName))
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

writeClientRespawn :: QuakeFile -> ClientRespawnT -> IO ()
writeClientRespawn saveFile clientRespawn = do
    writeClientPersistant saveFile (clientRespawn^.crCoopRespawn)
    writeInt              saveFile (clientRespawn^.crEnterFrame)
    writeInt              saveFile (clientRespawn^.crScore)
    writeVector           saveFile (clientRespawn^.crCmdAngles)
    writeBool             saveFile (clientRespawn^.crSpectator)

writePMoveState :: QuakeFile -> PMoveStateT -> IO ()
writePMoveState saveFile pMoveState = do
    writeInt saveFile (pMoveState^.pmsPMType)
    writeVectorShort saveFile (pMoveState^.pmsOrigin)
    writeVectorShort saveFile (pMoveState^.pmsVelocity)
    writeByte        saveFile (pMoveState^.pmsPMFlags)
    writeByte        saveFile (pMoveState^.pmsPMTime)
    writeShort       saveFile (pMoveState^.pmsGravity)
    writeShort       saveFile 0
    writeVectorShort saveFile (pMoveState^.pmsDeltaAngles)
