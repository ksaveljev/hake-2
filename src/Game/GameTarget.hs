module Game.GameTarget
    ( spTargetBlaster
    , spTargetChangeLevel
    , spTargetCrossLevelTarget
    , spTargetCrossLevelTrigger
    , spTargetEarthquake
    , spTargetExplosion
    , spTargetGoal
    , spTargetHelp
    , spTargetLaser
    , spTargetLightRamp
    , spTargetSecret
    , spTargetSpawner
    , spTargetSpeaker
    , spTargetSplash
    , spTargetTempEntity
    ) where

import           Control.Applicative   ((<|>))
import           Control.Lens          (use, (^.), (.=), (%=), (+=), (&), (.~), (%~))
import           Control.Monad         (when, void)
import           Data.Bits             (complement, (.&.), (.|.))
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import           Data.Char             (toLower)
import           Data.Maybe            (fromMaybe, isNothing)
import           Linear                (V3(..))

import qualified Constants
import           Game.CVarT
import           Game.EdictT
import           Game.EntityStateT
import qualified Game.GameBase         as GameBase
import qualified Game.GameCombat       as GameCombat
import           Game.GameLocalsT
import qualified Game.GameUtil         as GameUtil
import qualified Game.GameWeapon       as GameWeapon
import           Game.LevelLocalsT
import           Game.SpawnTempT
import qualified QCommon.Com           as Com
import           QCommon.CVarVariables
import           QuakeRef
import           QuakeState
import           Types
import qualified Util.Lib              as Lib

import {-# SOURCE #-}           Game.GameImportT
import {-# SOURCE #-} qualified Game.GameSpawn        as GameSpawn

spTargetBlaster :: Ref EdictT -> Quake ()
spTargetBlaster selfRef = do
    GameBase.setMoveDir selfRef =<< readRef selfRef
    soundIndex <- use (gameBaseGlobals.gbGameImport.giSoundIndex)
    soundIdx <- soundIndex (Just "weapons/laser2.wav")
    modifyRef selfRef (\v -> v & eUse .~ Just useTargetBlaster
                               & eNoiseIndex .~ soundIdx
                               & eDmg %~ (\d -> if d == 0 then 15 else d)
                               & eSpeed %~ (\s -> if s == 0 then 1000 else s)
                               & eSvFlags .~ Constants.svfNoClient)

useTargetBlaster :: EntUse
useTargetBlaster = EntUse "use_target_blaster" $ \selfRef _ _ -> do
    self <- readRef selfRef
    let effect | (self^.eSpawnFlags) .&. 2 /= 0 = 0
               | (self^.eSpawnFlags) .&. 1 /= 0 = Constants.efHyperblaster
               | otherwise                      = Constants.efBlaster
    GameWeapon.fireBlaster selfRef (self^.eEntityState.esOrigin) (self^.eMoveDir) (self^.eDmg) (truncate (self^.eSpeed)) Constants.efBlaster (Constants.modTargetBlaster /= 0) -- True
    sound <- use (gameBaseGlobals.gbGameImport.giSound)
    sound (Just selfRef) Constants.chanVoice (self^.eNoiseIndex) 1 Constants.attnNorm 0

spTargetChangeLevel :: Ref EdictT -> Quake ()
spTargetChangeLevel edictRef = do
    edict <- readRef edictRef
    maybe (noMap edict) (doSpawnTargetChangeLevel edict) (edict^.eMap)
  where
    noMap edict = do
        dprintf <- use (gameBaseGlobals.gbGameImport.giDprintf)
        dprintf (B.concat ["target_changelevel with no map at ", Lib.vtos (edict^.eEntityState.esOrigin), "\n"])
        GameUtil.freeEdict edictRef
    doSpawnTargetChangeLevel edict edictMap = do
        -- ugly hack because *SOMEBODY* screwed up their map
        mapName <- fmap (BC.map toLower) (use (gameBaseGlobals.gbLevel.llMapName))
        when (mapName == "fact1" && BC.map toLower edictMap == "fact3") $
            modifyRef edictRef (\v -> v & eMap .~ Just "fact3$secret1")
        modifyRef edictRef (\v -> v & eUse .~ Just useTargetChangeLevel
                                    & eSvFlags .~ Constants.svfNoClient)

useTargetChangeLevel :: EntUse
useTargetChangeLevel = error "GameTarget.useTargetChangeLevel" -- TODO

spTargetCrossLevelTarget :: Ref EdictT -> Quake ()
spTargetCrossLevelTarget selfRef = do
    self <- readRef selfRef
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    let delay = if (self^.eDelay) == 0 then 1 else self^.eDelay
    modifyRef selfRef (\v -> v & eDelay .~ delay
                               & eSvFlags .~ Constants.svfNoClient
                               & eThink .~ Just targetCrossLevelTargetThink
                               & eNextThink .~ levelTime + delay)

targetCrossLevelTargetThink :: EntThink
targetCrossLevelTargetThink = EntThink "target_crosslevel_target_think" $ \selfRef -> do
    self <- readRef selfRef
    serverFlags <- use (gameBaseGlobals.gbGame.glServerFlags)
    when ((self^.eSpawnFlags) == (serverFlags .&. Constants.sflCrossTriggerMask .&. (self^.eSpawnFlags))) $ do
        GameUtil.useTargets selfRef (Just selfRef)
        GameUtil.freeEdict selfRef
    return True

spTargetCrossLevelTrigger :: Ref EdictT -> Quake ()
spTargetCrossLevelTrigger selfRef =
    modifyRef selfRef (\v -> v & eSvFlags .~ Constants.svfNoClient
                               & eUse .~ Just triggerCrossLevelTriggerUse)

triggerCrossLevelTriggerUse :: EntUse
triggerCrossLevelTriggerUse = EntUse "trigger_crosslevel_trigger_use" $ \selfRef _ _ -> do
    self <- readRef selfRef
    gameBaseGlobals.gbGame.glServerFlags %= (.|. (self^.eSpawnFlags))
    GameUtil.freeEdict selfRef

spTargetEarthquake :: Ref EdictT -> Quake ()
spTargetEarthquake selfRef = do
    self <- readRef selfRef
    gameImport <- use $ gameBaseGlobals.gbGameImport
    when (isNothing (self^.eTargetName)) $
        (gameImport^.giDprintf) (B.concat ["untargeted ", self^.eClassName, " at ", Lib.vtos (self^.eEntityState.esOrigin), "\n"])
    noiseIndex <- (gameImport^.giSoundIndex) (Just "world/quake.wav")
    modifyRef selfRef (\v -> v & eCount %~ (\c -> if c == 0 then 5 else c)
                               & eSpeed %~ (\s -> if s == 0 then 200 else s)
                               & eSvFlags %~ (.|. Constants.svfNoClient)
                               & eThink .~ Just targetEarthquakeThink
                               & eUse .~ Just targetEarthquakeUse
                               & eNoiseIndex .~ noiseIndex)

targetEarthquakeThink :: EntThink
targetEarthquakeThink = error "GameTarget.targetEarthquakeThink" -- TODO

targetEarthquakeUse :: EntUse
targetEarthquakeUse = EntUse "target_earthquake_use" $ \selfRef _ activatorRef -> do
    self <- readRef selfRef
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    modifyRef selfRef (\v -> v & eTimeStamp .~ levelTime + fromIntegral (self^.eCount)
                               & eNextThink .~ levelTime + Constants.frameTime
                               & eActivator .~ activatorRef
                               & eLastMoveTime .~ 0)

spTargetExplosion :: Ref EdictT -> Quake ()
spTargetExplosion edictRef =
    modifyRef edictRef (\v -> v & eUse .~ Just useTargetExplosion
                                & eSvFlags .~ Constants.svfNoClient)

useTargetExplosion :: EntUse
useTargetExplosion = EntUse "use_target_explosion" $ \selfRef _ activatorRef -> do
    modifyRef selfRef (\v -> v & eActivator .~ activatorRef)
    self <- readRef selfRef
    doUseTargetExplosion selfRef self
  where
    doUseTargetExplosion selfRef self
        | (self^.eDelay) == 0 =
            void (entThink targetExplosionExplode selfRef)
        | otherwise = do
            levelTime <- use (gameBaseGlobals.gbLevel.llTime)
            modifyRef selfRef (\v -> v & eThink .~ Just targetExplosionExplode
                                       & eNextThink .~ levelTime + (self^.eDelay))


targetExplosionExplode :: EntThink
targetExplosionExplode = EntThink "target_explosion_explode" $ \selfRef -> do
    self <- readRef selfRef
    gameImport <- use (gameBaseGlobals.gbGameImport)
    (gameImport^.giWriteByte) Constants.svcTempEntity
    (gameImport^.giWriteByte) Constants.teExplosion1
    (gameImport^.giWritePosition) (self^.eEntityState.esOrigin)
    (gameImport^.giMulticast) (self^.eEntityState.esOrigin) Constants.multicastPhs
    maybe activatorError (doRadiusDamage selfRef self) (self^.eActivator)
    modifyRef selfRef (\v -> v & eDelay .~ 0)
    GameUtil.useTargets selfRef (self^.eActivator)
    modifyRef selfRef (\v -> v & eDelay .~ (self^.eDelay))
    return True
  where
    activatorError = Com.fatalError "GameTarget.targetExplosionExplode self^.eActivator is Nothing"
    doRadiusDamage selfRef self activatorRef =
        GameCombat.radiusDamage selfRef 
                                activatorRef
                                (fromIntegral (self^.eDmg))
                                Nothing
                                (fromIntegral (self^.eDmg) + 40)
                                Constants.modExplosive

spTargetGoal :: Ref EdictT -> Quake ()
spTargetGoal edictRef = do
    deathmatch <- fmap (^.cvValue) deathmatchCVar
    doSpawnTargetGoal deathmatch
  where
    doSpawnTargetGoal deathmatch
        | deathmatch /= 0 = -- auto-remove for deathmatch
            GameUtil.freeEdict edictRef
        | otherwise = do
            modifyRef edictRef (\v -> v & eUse .~ Just useTargetGoal)
            noise <- getNoise
            soundIndex <- use (gameBaseGlobals.gbGameImport.giSoundIndex)
            noiseIdx <- soundIndex noise
            modifyRef edictRef (\v -> v & eSvFlags .~ Constants.svfNoClient
                                        & eNoiseIndex .~ noiseIdx)
            gameBaseGlobals.gbLevel.llTotalGoals += 1
    getNoise = do
        noise <- (use $ gameBaseGlobals.gbSpawnTemp.stNoise) 
        maybe (return (Just "misc/secret.wav")) (return . Just) noise

useTargetGoal :: EntUse
useTargetGoal = EntUse "use_target_goal" $ \edictRef _ activatorRef -> do
    edict <- readRef edictRef
    gameImport <- use (gameBaseGlobals.gbGameImport)
    (gameImport^.giSound) (Just edictRef) Constants.chanVoice (edict^.eNoiseIndex) 1 Constants.attnNorm 0
    gameBaseGlobals.gbLevel.llFoundGoals += 1
    level <- use (gameBaseGlobals.gbLevel)
    when ((level^.llFoundGoals) == (level^.llTotalGoals)) $
        (gameImport^.giConfigString) Constants.csCdTrack "0"
    GameUtil.useTargets edictRef activatorRef
    GameUtil.freeEdict edictRef

spTargetHelp :: Ref EdictT -> Quake ()
spTargetHelp edictRef = do
    deathmatch <- fmap (^.cvValue) deathmatchCVar
    doSpawnTargetHelp deathmatch
  where
    doSpawnTargetHelp deathmatch
        | deathmatch /= 0 = -- auto-remove for deathmatch
            GameUtil.freeEdict edictRef
        | otherwise = do
            edict <- readRef edictRef
            maybe (noMessage edict) (\_ -> updateEdict) (edict^.eMessage)
    noMessage edict = do
        dprintf <- use (gameBaseGlobals.gbGameImport.giDprintf)
        dprintf (B.concat [edict^.eClassName, " with no message at ", Lib.vtos (edict^.eEntityState.esOrigin), "\n"])
        GameUtil.freeEdict edictRef
    updateEdict =
        modifyRef edictRef (\v -> v & eUse .~ Just useTargetHelp)

useTargetHelp :: EntUse
useTargetHelp = EntUse "Use_Target_Help" $ \edictRef _ _ -> do
    edict <- readRef edictRef
    maybe messageError (setHelpMessage (edict^.eSpawnFlags)) (edict^.eMessage)
    gameBaseGlobals.gbGame.glHelpChanged += 1
  where
    messageError = Com.fatalError "GameTarget.useTargetHelp edict^.eMessage is Nothing"
    setHelpMessage :: Int -> B.ByteString -> Quake ()
    setHelpMessage spawnFlags msg
        | spawnFlags .&. 1 /= 0 =
            gameBaseGlobals.gbGame.glHelpMessage1 .= msg
        | otherwise =
            gameBaseGlobals.gbGame.glHelpMessage2 .= msg

spTargetLaser :: Ref EdictT -> Quake ()
spTargetLaser selfRef = do
    -- let everything else get spawned before we start firing
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    modifyRef selfRef (\v -> v & eThink .~ Just targetLaserStart
                               & eNextThink .~ levelTime + 1)

targetLaserStart :: EntThink
targetLaserStart = EntThink "target_laser_start" $ \selfRef -> do
    self <- readRef selfRef
        -- set the beam diameter
    let frame | (self^.eSpawnFlags) .&. 64 /= 0 = 16
              | otherwise                       = 4
        -- set the color
        skinNum | (self^.eSpawnFlags) .&.  2 /= 0 = 0xF2F2F0F0
                | (self^.eSpawnFlags) .&.  4 /= 0 = 0xD0D1D2D3
                | (self^.eSpawnFlags) .&.  8 /= 0 = 0xF3F3F1F1
                | (self^.eSpawnFlags) .&. 16 /= 0 = 0xDCDDDEDF
                | (self^.eSpawnFlags) .&. 32 /= 0 = 0xE0E1E2E3
                | otherwise                       = self^.eEntityState.esSkinNum
    modifyRef selfRef (\v -> v & eMoveType .~ Constants.moveTypeNone
                               & eSolid .~ Constants.solidNot
                               & eEntityState.esRenderFx %~ (.|. (Constants.rfBeam .|. Constants.rfTranslucent))
                               & eEntityState.esModelIndex .~ 1 -- must be non-zero
                               & eEntityState.esFrame .~ frame
                               & eEntityState.esSkinNum .~ skinNum)
    when (isNothing (self^.eEnemy)) $
        maybe (noTarget selfRef) (hasTarget selfRef self) (self^.eTarget)
    modifyRef selfRef (\v -> v & eUse .~ Just targetLaserUse
                               & eThink .~ Just targetLaserThink
                               & eDmg %~ (\d -> if d == 0 then 1 else d)
                               & eMins .~ V3 (-8) (-8) (-8)
                               & eMaxs .~ V3 8 8 8)
    linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
    linkEntity selfRef
    laserOnOff selfRef =<< readRef selfRef
    return True
  where
    noTarget selfRef = GameBase.setMoveDir selfRef =<< readRef selfRef
    hasTarget selfRef self target = do
        foundTarget <- GameBase.gFind Nothing GameBase.findByTarget target
        when (isNothing foundTarget) $ do
            dprintf <- use (gameBaseGlobals.gbGameImport.giDprintf)
            dprintf (B.concat [self^.eClassName, " at ", Lib.vtos (self^.eEntityState.esOrigin), ": ", target, " is a bad target\n"])
        modifyRef selfRef (\v -> v & eEnemy .~ foundTarget)
    laserOnOff selfRef self
        | (self^.eSpawnFlags) .&. 1 /= 0 = targetLaserOn selfRef
        | otherwise                      = targetLaserOff selfRef

targetLaserUse :: EntUse
targetLaserUse = EntUse "target_laser_use" $ \selfRef _ activatorRef -> do
    modifyRef selfRef (\v -> v & eActivator .~ activatorRef)
    self <- readRef selfRef
    laserOnOff selfRef self
  where
    laserOnOff selfRef self
        | (self^.eSpawnFlags) .&. 1 /= 0 = targetLaserOff selfRef
        | otherwise                      = targetLaserOn selfRef

targetLaserThink :: EntThink
targetLaserThink = error "GameTarget.targetLaserThink" -- TODO

targetLaserOn :: Ref EdictT -> Quake ()
targetLaserOn selfRef = do
    self <- readRef selfRef
    let activator = fromMaybe selfRef (self^.eActivator)
    modifyRef selfRef (\v -> v & eActivator .~ Just activator
                               & eSpawnFlags %~ (.|. 0x80000001)
                               & eSvFlags %~ (.&. (complement Constants.svfNoClient)))
    void (entThink targetLaserThink selfRef)

targetLaserOff :: Ref EdictT -> Quake ()
targetLaserOff selfRef =
    modifyRef selfRef (\v -> v & eSpawnFlags %~ (.&. (complement 1))
                               & eSvFlags %~ (.|. Constants.svfNoClient)
                               & eNextThink .~ 0)

spTargetLightRamp :: Ref EdictT -> Quake ()
spTargetLightRamp = error "GameTarget.spTargetLightRamp" -- TODO

spTargetSecret :: Ref EdictT -> Quake ()
spTargetSecret edictRef = do
    deathmatch <- fmap (^.cvValue) deathmatchCVar
    doSpawnTargetSecret deathmatch
  where
    doSpawnTargetSecret deathmatch
        | deathmatch /= 0 = -- auto-remove for deathmatch
            GameUtil.freeEdict edictRef
        | otherwise = do
            gameBaseGlobals.gbSpawnTemp.stNoise %= checkNoise
            noise <- use (gameBaseGlobals.gbSpawnTemp.stNoise)
            soundIndex <- use (gameBaseGlobals.gbGameImport.giSoundIndex)
            noiseIndex <- soundIndex noise
            modifyRef edictRef (\v -> v & eNoiseIndex .~ noiseIndex
                                        & eUse .~ Just useTargetSecret
                                        & eSvFlags .~ Constants.svfNoClient)
            gameBaseGlobals.gbLevel.llTotalSecrets += 1
            -- map bug hack
            edict <- readRef edictRef
            mapName <- fmap (BC.map toLower) (use (gameBaseGlobals.gbLevel.llMapName))
            when ("mine3" == mapName && (edict^.eEntityState.esOrigin) == V3 280 (-2048) (-624)) $
                modifyRef edictRef (\v -> v & eMessage .~ Just "You have found a secret area.")
    checkNoise Nothing = Just "misc/secret.wav"
    checkNoise noise = noise

useTargetSecret :: EntUse
useTargetSecret = EntUse "use_target_secret" $ \edictRef _ activatorRef -> do
    edict <- readRef edictRef
    sound <- use (gameBaseGlobals.gbGameImport.giSound)
    sound (Just edictRef) Constants.chanVoice (edict^.eNoiseIndex) 1 Constants.attnNorm 0
    gameBaseGlobals.gbLevel.llFoundSecrets += 1
    GameUtil.useTargets edictRef activatorRef
    GameUtil.freeEdict edictRef

spTargetSpawner :: Ref EdictT -> Quake ()
spTargetSpawner selfRef = do
    modifyRef selfRef (\v -> v & eUse .~ Just useTargetSpawner
                               & eSvFlags .~ Constants.svfNoClient)
    self <- readRef selfRef
    when ((self^.eSpeed) /= 0) $ do
        GameBase.setMoveDir selfRef self
        modifyRef selfRef (\v -> v & eMoveDir %~ fmap (* (self^.eSpeed)))

useTargetSpawner :: EntUse
useTargetSpawner = EntUse "use_target_spawner" $ \selfRef _ _ -> do
    self <- readRef selfRef
    className <- getClassName (self^.eTarget)
    edictRef <- GameUtil.spawn
    modifyRef edictRef (\v -> v & eClassName .~ className
                                & eEntityState.esOrigin .~ (self^.eEntityState.esOrigin)
                                & eEntityState.esAngles .~ (self^.eEntityState.esAngles))
    GameSpawn.callSpawn edictRef
    gameImport <- use (gameBaseGlobals.gbGameImport)
    (gameImport^.giUnlinkEntity) edictRef
    void (GameUtil.killBox edictRef)
    (gameImport^.giLinkEntity) edictRef
    updatedSelf <- readRef selfRef
    when ((updatedSelf^.eSpeed) /= 0) $
        modifyRef edictRef (\v -> v & eVelocity .~ (updatedSelf^.eMoveDir))
  where
    getClassName Nothing = do
        Com.fatalError "GameTarget.useTargetSpawner self^.eTarget is Nothing"
        return B.empty
    getClassName (Just target) = do return target

spTargetSpeaker :: Ref EdictT -> Quake ()
spTargetSpeaker edictRef = do
    noise <- use (gameBaseGlobals.gbSpawnTemp.stNoise)
{-
    let dprintf = gameImport^.giDprintf
        soundIndex = gameImport^.giSoundIndex
        linkEntity = gameImport^.giLinkEntity
-}
    edict <- readRef edictRef
    doSpawnTargetSpeaker noise edict
  where
    doSpawnTargetSpeaker Nothing edict = do
        dprintf <- use (gameBaseGlobals.gbGameImport.giDprintf)
        dprintf (B.concat ["target_speaker with no noise set at ", Lib.vtos (edict^.eEntityState.esOrigin), "\n"])
    doSpawnTargetSpeaker (Just noiseStr) edict = do
        let buffer | ".wav" `B.isInfixOf` noiseStr = noiseStr
                   | otherwise = noiseStr `B.append` ".wav"
        gameImport <- use (gameBaseGlobals.gbGameImport)
        noiseIndex <- (gameImport^.giSoundIndex) (Just buffer)
        modifyRef edictRef (\v -> v & eNoiseIndex .~ noiseIndex)
        when ((edict^.eVolume) == 0) $
            modifyRef edictRef (\v -> v & eVolume .~ 1)
        when ((edict^.eAttenuation) == 0) $
            modifyRef edictRef (\v -> v & eAttenuation .~ 1)
        when ((edict^.eAttenuation) == (-1)) $
            modifyRef edictRef (\v -> v & eAttenuation .~ 0)
        -- check for prestarted looping sound
        when ((edict^.eSpawnFlags) .&. 1 /= 0) $
            modifyRef edictRef (\v -> v & eEntityState.esSound .~ noiseIndex)
        modifyRef edictRef (\v -> v & eUse .~ Just useTargetSpeaker)
        -- must link the entity so we get areas and clusters so
        -- the server can determine who to send updates to
        (gameImport^.giLinkEntity) edictRef

useTargetSpeaker :: EntUse
useTargetSpeaker = EntUse "Use_Target_Speaker" $ \edictRef _ _ -> do
    edict <- readRef edictRef
    doUseTargetSpeaker edictRef edict
  where
    doUseTargetSpeaker edictRef edict
        | (edict^.eSpawnFlags) .&. 3 /= 0 = -- looping sound toggles
            modifyRef edictRef (\v -> v & eEntityState.esSound .~ (if (edict^.eEntityState.esSound) /= 0 then 0 else edict^.eNoiseIndex))
        | otherwise = do -- normal sound
            let chan | (edict^.eSpawnFlags) .&. 4 /= 0 = Constants.chanVoice .|. Constants.chanReliable
                     | otherwise                       = Constants.chanVoice
            -- use a positioned_sound, because this entity won't normally be
            -- sent to any clients because it is invisible
            positionedSound <- use (gameBaseGlobals.gbGameImport.giPositionedSound)
            positionedSound (Just (edict^.eEntityState.esOrigin)) edictRef chan (edict^.eNoiseIndex) (edict^.eVolume) (edict^.eAttenuation) 0

spTargetSplash :: Ref EdictT -> Quake ()
spTargetSplash edictRef = do
    modifyRef edictRef (\v -> v & eUse .~ Just useTargetSplash)
    GameBase.setMoveDir edictRef =<< readRef edictRef
    edict <- readRef edictRef
    when ((edict^.eCount) == 0) $
        modifyRef edictRef (\v -> v & eCount .~ 32)
    modifyRef edictRef (\v -> v & eSvFlags .~ Constants.svfNoClient)

useTargetSplash :: EntUse
useTargetSplash = EntUse "use_target_splash" $ \selfRef _ activatorRef -> do
    gameImport <- use (gameBaseGlobals.gbGameImport)
    self <- readRef selfRef
    (gameImport^.giWriteByte) (fromIntegral Constants.svcTempEntity)
    (gameImport^.giWriteByte) (fromIntegral Constants.teSplash)
    (gameImport^.giWriteByte) (fromIntegral (self^.eCount))
    (gameImport^.giWritePosition) (self^.eEntityState.esOrigin)
    (gameImport^.giWriteDir) (self^.eMoveDir)
    (gameImport^.giWriteByte) (fromIntegral (self^.eSounds))
    (gameImport^.giMulticast) (self^.eEntityState.esOrigin) Constants.multicastPvs
    when ((self^.eDmg) /= 0) $
        maybe activatorError (doRadiusDamage selfRef self) activatorRef
  where
    activatorError = Com.fatalError "GameTarget.useTargetSplash activatorRef is Nothing"
    doRadiusDamage selfRef self activatorRef =
        GameCombat.radiusDamage selfRef activatorRef (fromIntegral (self^.eDmg)) Nothing (fromIntegral (self^.eDmg) + 40) Constants.modSplash

spTargetTempEntity :: Ref EdictT -> Quake ()
spTargetTempEntity edictRef = modifyRef edictRef (\v -> v & eUse .~ Just useTargetTEnt)

useTargetTEnt :: EntUse
useTargetTEnt = EntUse "Use_Target_Tent" $ \edictRef _ _ -> do
    edict <- readRef edictRef
    gameImport <- use (gameBaseGlobals.gbGameImport)
    (gameImport^.giWriteByte) Constants.svcTempEntity
    (gameImport^.giWriteByte) (edict^.eStyle)
    (gameImport^.giWritePosition) (edict^.eEntityState.esOrigin)
    (gameImport^.giMulticast) (edict^.eEntityState.esOrigin) Constants.multicastPvs
