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
import           Data.Bits             ((.&.), (.|.))
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
import {-# SOURCE #-} Game.GameImportT
import           Game.GameLocalsT
import {-# SOURCE #-} qualified Game.GameSpawn        as GameSpawn
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

spTargetBlaster :: Ref' EdictT -> Quake ()
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
    {- WHAT IS THIS FOR? Original quake2 also has this variable which is not used anywhere
    let effect = if | (self^.eSpawnFlags) .&. 2 /= 0 -> 0
                    | (self^.eSpawnFlags) .&. 1 /= 0 -> Constants.efHyperblaster
                    | otherwise -> Constants.efBlaster
    -}
    GameWeapon.fireBlaster selfRef (self^.eEntityState.esOrigin) (self^.eMoveDir) (self^.eDmg) (truncate (self^.eSpeed)) Constants.efBlaster (Constants.modTargetBlaster /= 0) -- True
    sound <- use (gameBaseGlobals.gbGameImport.giSound)
    sound (Just selfRef) Constants.chanVoice (self^.eNoiseIndex) 1 Constants.attnNorm 0

spTargetChangeLevel :: Ref' EdictT -> Quake ()
spTargetChangeLevel edictRef = do
    edict <- readRef edictRef
    targetChangeLevel edict (edict^.eMap)
  where
    targetChangeLevel edict Nothing = do
        dprintf <- use (gameBaseGlobals.gbGameImport.giDprintf)
        dprintf (B.concat ["target_changelevel with no map at ", Lib.vtos (edict^.eEntityState.esOrigin), "\n"])
        GameUtil.freeEdict edictRef
    targetChangeLevel edict _ = do
        -- ugly hack because *SOMEBODY* screwed up their map
        mapName <- fmap (BC.map toLower) (use (gameBaseGlobals.gbLevel.llMapName))
        when (mapName == "fact1" && fmap (BC.map toLower) (edict^.eMap) == Just "fact3") $
            modifyRef edictRef (\v -> v & eMap .~ Just "fact3$secret1")
        modifyRef edictRef (\v -> v & eUse .~ Just useTargetChangeLevel
                                    & eSvFlags .~ Constants.svfNoClient)

useTargetChangeLevel :: EntUse
useTargetChangeLevel = EntUse "use_target_changelevel" $ \_ _ _ -> do
    error "GameTarget.useTargetChangeLevel" -- TODO

spTargetCrossLevelTarget :: Ref' EdictT -> Quake ()
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

spTargetCrossLevelTrigger :: Ref' EdictT -> Quake ()
spTargetCrossLevelTrigger selfRef =
    modifyRef selfRef (\v -> v & eSvFlags .~ Constants.svfNoClient
                               & eUse .~ Just triggerCrossLevelTriggerUse)

triggerCrossLevelTriggerUse :: EntUse
triggerCrossLevelTriggerUse = EntUse "trigger_crosslevel_trigger_use" $ \selfRef _ _ -> do
    self <- readRef selfRef
    gameBaseGlobals.gbGame.glServerFlags %= (.|. (self^.eSpawnFlags))
    GameUtil.freeEdict selfRef

spTargetEarthquake :: Ref' EdictT -> Quake ()
spTargetEarthquake selfRef = do
    self <- readRef selfRef
    when (isNothing (self^.eTargetName)) $ do
        dprintf <- use (gameBaseGlobals.gbGameImport.giDprintf)
        dprintf (B.concat ["untargeted ", self^.eClassName, " at ", Lib.vtos (self^.eEntityState.esOrigin), "\n"])
    soundIndex <- use (gameBaseGlobals.gbGameImport.giSoundIndex)
    noiseIndex <- soundIndex (Just "world/quake.wav")
    modifyRef selfRef (\v -> v & eCount %~ (\c -> if c == 0 then 5 else c)
                               & eSpeed %~ (\s -> if s == 0 then 200 else s)
                               & eSvFlags %~ (.|. Constants.svfNoClient)
                               & eThink .~ Just targetEarthquakeThink
                               & eUse .~ Just targetEarthquakeUse
                               & eNoiseIndex .~ noiseIndex)

targetEarthquakeThink :: EntThink
targetEarthquakeThink = EntThink "target_earthquake_think" $ \selfRef -> do
    error "GameTarget.targetEarthquakeThink" -- TODO

targetEarthquakeUse :: EntUse
targetEarthquakeUse = EntUse "target_earthquake_use" $ \selfRef _ activatorRef -> do
    self <- readRef selfRef
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    modifyRef selfRef (\v -> v & eTimeStamp .~ levelTime + fromIntegral (self^.eCount)
                               & eNextThink .~ levelTime + Constants.frameTime
                               & eActivator .~ activatorRef
                               & eLastMoveTime .~ 0)

spTargetExplosion :: Ref' EdictT -> Quake ()
spTargetExplosion edictRef = do
    modifyRef edictRef (\v -> v & eUse .~ Just useTargetExplosion
                                & eSvFlags .~ Constants.svfNoClient)

useTargetExplosion :: EntUse
useTargetExplosion = EntUse "use_target_explosion" $ \selfRef _ activatorRef -> do
    modifyRef selfRef (\v -> v & eActivator .~ activatorRef)
    self <- readRef selfRef
    targetExplosion selfRef self
  where
    targetExplosion selfRef self
        | (self^.eDelay) == 0 =
            void (entThink targetExplosionExplode selfRef)
        | otherwise = do
            levelTime <- use (gameBaseGlobals.gbLevel.llTime)
            modifyRef selfRef (\v -> v & eThink .~ Just targetExplosionExplode
                                       & eNextThink .~ levelTime + (self^.eDelay))

targetExplosionExplode :: EntThink
targetExplosionExplode = EntThink "target_explosion_explode" $ \selfRef -> do
    gameImport <- use (gameBaseGlobals.gbGameImport)
    let writeByte = gameImport^.giWriteByte
        writePosition = gameImport^.giWritePosition
        multicast = gameImport^.giMulticast
    self <- readRef selfRef
    writeByte Constants.svcTempEntity
    writeByte Constants.teExplosion1
    writePosition (self^.eEntityState.esOrigin)
    multicast (self^.eEntityState.esOrigin) Constants.multicastPhs
    doDamage selfRef self (self^.eActivator)
    let savedDelay = self^.eDelay
    modifyRef selfRef (\v -> v & eDelay .~ 0)
    GameUtil.useTargets selfRef (self^.eActivator)
    modifyRef selfRef (\v -> v & eDelay .~ savedDelay)
    return True
  where
    doDamage _ _ Nothing =
        Com.fatalError "GameTarget.targetExplosionExplode self^.eActivator is Nothing"
    doDamage selfRef self (Just activatorRef) =
        GameCombat.radiusDamage selfRef activatorRef (fromIntegral (self^.eDmg)) Nothing (fromIntegral (self^.eDmg) + 40) Constants.modExplosive

spTargetGoal :: Ref' EdictT -> Quake ()
spTargetGoal edictRef = do
    deathmatch <- fmap (^.cvValue) deathmatchCVar
    targetGoal deathmatch
  where
    targetGoal deathmatch
        | deathmatch /= 0 =
            GameUtil.freeEdict edictRef
        | otherwise = do
            modifyRef edictRef (\v -> v & eUse .~ Just useTargetGoal)
            noise <- fmap (fromMaybe "misc/secret.wav") (use (gameBaseGlobals.gbSpawnTemp.stNoise))
            soundIndex <- use (gameBaseGlobals.gbGameImport.giSoundIndex)
            noiseIdx <- soundIndex (Just noise)
            modifyRef edictRef (\v -> v & eSvFlags .~ Constants.svfNoClient
                                        & eNoiseIndex .~ noiseIdx)
            gameBaseGlobals.gbLevel.llTotalGoals += 1

useTargetGoal :: EntUse
useTargetGoal = EntUse "use_target_goal" $ \edictRef _ activatorRef -> do
    edict <- readRef edictRef
    sound <- use (gameBaseGlobals.gbGameImport.giSound)
    sound (Just edictRef) Constants.chanVoice (edict^.eNoiseIndex) 1 Constants.attnNorm 0
    gameBaseGlobals.gbLevel.llFoundGoals += 1
    level <- use (gameBaseGlobals.gbLevel)
    when ((level^.llFoundGoals) == (level^.llTotalGoals)) $ do
        configString <- use (gameBaseGlobals.gbGameImport.giConfigString)
        configString Constants.csCdTrack "0"
    GameUtil.useTargets edictRef activatorRef
    GameUtil.freeEdict edictRef

spTargetHelp :: Ref' EdictT -> Quake ()
spTargetHelp edictRef = do
    deathmatch <- fmap (^.cvValue) deathmatchCVar
    targetHelp deathmatch
  where
    targetHelp deathmatch
        | deathmatch /= 0 =
            GameUtil.freeEdict edictRef
        | otherwise = do
            edict <- readRef edictRef
            maybe (noMessage edict) setUseTargetHelp (edict^.eMessage)
    noMessage edict = do
        dprintf <- use (gameBaseGlobals.gbGameImport.giDprintf)
        dprintf (B.concat [edict^.eClassName, " with no message at ", Lib.vtos (edict^.eEntityState.esOrigin), "\n"])
        GameUtil.freeEdict edictRef
    setUseTargetHelp _ =
        modifyRef edictRef (\v -> v & eUse .~ Just useTargetHelp)

useTargetHelp :: EntUse
useTargetHelp = EntUse "Use_Target_Help" $ \edictRef _ _ -> do
    edict <- readRef edictRef
    targetHelp edict
    gameBaseGlobals.gbGame.glHelpChanged += 1
  where
    targetHelp :: EdictT -> Quake ()
    targetHelp edict
        | (edict^.eSpawnFlags) .&. 1 /= 0 =
            gameBaseGlobals.gbGame.glHelpMessage1 .= fromMaybe "" (edict^.eMessage) -- TODO: are we sure about ""?
        | otherwise =
            gameBaseGlobals.gbGame.glHelpMessage2 .= fromMaybe "" (edict^.eMessage) -- TODO: are we sure about ""?

spTargetLaser :: Ref' EdictT -> Quake ()
spTargetLaser selfRef = do
    -- let everything else get spawned before we start firing
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    modifyRef selfRef (\v -> v & eThink .~ Just targetLaserStart
                               & eNextThink .~ levelTime + 1)

targetLaserStart :: EntThink
targetLaserStart = EntThink "target_laser_start" $ \selfRef -> do
    error "GameTarget.targetLaserStart" -- TODO

spTargetLightRamp :: Ref' EdictT -> Quake ()
spTargetLightRamp = error "GameTarget.spTargetLightRamp" -- TODO

spTargetSecret :: Ref' EdictT -> Quake ()
spTargetSecret edictRef = do
    deathmatch <- fmap (^.cvValue) deathmatchCVar
    targetSecret deathmatch
  where
    targetSecret deathmatch
        | deathmatch /= 0 =
            GameUtil.freeEdict edictRef
        | otherwise = do
            gameBaseGlobals.gbSpawnTemp.stNoise %= \v -> v <|> Just "misc/secret.wav"
            noise <- use (gameBaseGlobals.gbSpawnTemp.stNoise)
            soundIndex <- use (gameBaseGlobals.gbGameImport.giSoundIndex)
            noiseIdx <- soundIndex noise
            modifyRef edictRef (\v -> v & eNoiseIndex .~ noiseIdx
                                        & eUse .~ Just useTargetSecret
                                        & eSvFlags .~ Constants.svfNoClient)
            gameBaseGlobals.gbLevel.llTotalSecrets += 1
            -- map bug hack
            edict <- readRef edictRef
            mapName <- fmap (BC.map toLower) (use (gameBaseGlobals.gbLevel.llMapName))
            when ("mine3" == mapName && (edict^.eEntityState.esOrigin) == V3 280 (-2048) (-624)) $
                modifyRef edictRef (\v -> v & eMessage .~ Just "You have found a secret area.")

useTargetSecret :: EntUse
useTargetSecret = EntUse "use_target_secret" $ \edictRef _ activatorRef -> do
    edict <- readRef edictRef
    sound <- use (gameBaseGlobals.gbGameImport.giSound)
    sound (Just edictRef) Constants.chanVoice (edict^.eNoiseIndex) 1 Constants.attnNorm 0
    gameBaseGlobals.gbLevel.llFoundSecrets += 1
    GameUtil.useTargets edictRef activatorRef
    GameUtil.freeEdict edictRef

spTargetSpawner :: Ref' EdictT -> Quake ()
spTargetSpawner selfRef = do
    modifyRef selfRef (\v -> v & eUse .~ Just useTargetSpawner
                               & eSvFlags .~ Constants.svfNoClient)
    self <- readRef selfRef
    when ((self^.eSpeed) /= 0) $ do
      GameBase.setMoveDir selfRef =<< readRef selfRef
      modifyRef selfRef (\v -> v & eMoveDir %~ fmap (* (self^.eSpeed)))

useTargetSpawner :: EntUse
useTargetSpawner = EntUse "use_target_spawner" $ \selfRef _ _ -> do
    self <- readRef selfRef
    edictRef <- GameUtil.spawn
    modifyRef edictRef (\v -> v & eClassName .~ fromMaybe "" (self^.eTarget) -- TODO: something other than "" ? should we error out on Nothing as usual?
                                & eEntityState.esOrigin .~ (self^.eEntityState.esOrigin)
                                & eEntityState.esAngles .~ (self^.eEntityState.esAngles))
    GameSpawn.callSpawn edictRef
    unlinkEntity <- use (gameBaseGlobals.gbGameImport.giUnlinkEntity)
    unlinkEntity edictRef
    void (GameUtil.killBox edictRef)
    linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
    linkEntity edictRef
    self' <- readRef selfRef
    when ((self'^.eSpeed) /= 0) $
        modifyRef edictRef (\v -> v & eVelocity .~ (self'^.eMoveDir))

spTargetSpeaker :: Ref' EdictT -> Quake ()
spTargetSpeaker edictRef = do
    edict <- readRef edictRef
    noise <- use (gameBaseGlobals.gbSpawnTemp.stNoise)
    maybe (noNoise edict) (targetSpeaker edict) noise
  where
    noNoise edict = do
        dprintf <- use (gameBaseGlobals.gbGameImport.giDprintf)
        dprintf (B.concat ["target_speaker with no noise set at ", Lib.vtos (edict^.eEntityState.esOrigin), "\n"])
    targetSpeaker edict noise = do
        soundIndex <- use (gameBaseGlobals.gbGameImport.giSoundIndex)
        noiseIdx <- soundIndex (Just (getBuffer noise))
        modifyRef edictRef (\v -> v & eNoiseIndex .~ noiseIdx
                                    & eVolume %~ (\n -> if n == 0 then 1 else n)
                                    & eAttenuation %~ (\n -> if n == 0 then 1 else n)
                                    & eAttenuation %~ (\n -> if n == -1 then 0 else n))
        -- check for prestarted looping sound
        when ((edict^.eSpawnFlags) .&. 1 /= 0) $
            modifyRef edictRef (\v -> v & eEntityState.esSound .~ noiseIdx)
        modifyRef edictRef (\v -> v & eUse .~ Just useTargetSpeaker)
        -- must link the entity so we get areas and clusters so
        -- the server can determine who to send updates to
        linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
        linkEntity edictRef
    getBuffer noise
        | ".wav" `B.isInfixOf` noise = noise
        | otherwise                  = noise `B.append` ".wav"

useTargetSpeaker :: EntUse
useTargetSpeaker = EntUse "Use_Target_Speaker" $ \edictRef _ _ -> do
    edict <- readRef edictRef
    targetSpeaker edictRef edict
  where
    targetSpeaker edictRef edict
        | (edict^.eSpawnFlags) .&. 3 /= 0 = -- looping sound toggles
            setSound edictRef edict
        | otherwise = do -- normal sound
            -- use a positioned_sound, because this entity won't normally be
            -- sent to any clients because it is invisible
            positionedSound <- use (gameBaseGlobals.gbGameImport.giPositionedSound)
            positionedSound (Just (edict^.eEntityState.esOrigin)) edictRef (getChan edict) (edict^.eNoiseIndex) (edict^.eVolume) (edict^.eAttenuation) 0
    setSound edictRef edict
        | (edict^.eEntityState.esSound) /= 0 =
            modifyRef edictRef (\v -> v & eEntityState.esSound .~ 0) -- turn it off
        | otherwise =
            modifyRef edictRef (\v -> v & eEntityState.esSound .~ (edict^.eNoiseIndex)) -- start it
    getChan edict
        | (edict^.eSpawnFlags) .&. 4 /= 0 = Constants.chanVoice .|. Constants.chanReliable
        | otherwise                       = Constants.chanVoice

spTargetSplash :: Ref' EdictT -> Quake ()
spTargetSplash edictRef = do
    modifyRef edictRef (\v -> v & eUse .~ Just useTargetSplash)
    edict <- readRef edictRef
    GameBase.setMoveDir edictRef edict
    when ((edict^.eCount) == 0) $
        modifyRef edictRef (\v -> v & eCount .~ 32)
    modifyRef edictRef (\v -> v & eSvFlags .~ Constants.svfNoClient)

useTargetSplash :: EntUse
useTargetSplash = EntUse "use_target_splash" $ \selfRef _ activatorRef -> do
    self <- readRef selfRef
    gameImport <- use (gameBaseGlobals.gbGameImport)
    let writeByte = gameImport^.giWriteByte
        writePosition = gameImport^.giWritePosition
        writeDir = gameImport^.giWriteDir
        multicast = gameImport^.giMulticast
    writeByte (fromIntegral Constants.svcTempEntity)
    writeByte (fromIntegral Constants.teSplash)
    writeByte (fromIntegral (self^.eCount))
    writePosition (self^.eEntityState.esOrigin)
    writeDir (self^.eMoveDir)
    writeByte (fromIntegral (self^.eSounds))
    multicast (self^.eEntityState.esOrigin) Constants.multicastPvs
    when ((self^.eDmg) /= 0) $
        doDamage selfRef self activatorRef
  where
    doDamage _ _ Nothing =
        Com.fatalError "GameTarget.useTargetSplash activatorRef is Nothing"
    doDamage selfRef self (Just activatorRef) =
        GameCombat.radiusDamage selfRef activatorRef (fromIntegral (self^.eDmg)) Nothing (fromIntegral (self^.eDmg) + 40) Constants.modSplash

spTargetTempEntity :: Ref' EdictT -> Quake ()
spTargetTempEntity edictRef =
    modifyRef edictRef (\v -> v & eUse .~ Just useTargetTEnt)

useTargetTEnt :: EntUse
useTargetTEnt = EntUse "Use_Target_Tent" $ \edictRef _ _ -> do
    edict <- readRef edictRef
    gameImport <- use (gameBaseGlobals.gbGameImport)
    let writeByte = gameImport^.giWriteByte
        writePosition = gameImport^.giWritePosition
        multicast = gameImport^.giMulticast
    writeByte Constants.svcTempEntity
    writeByte (edict^.eStyle)
    writePosition (edict^.eEntityState.esOrigin)
    multicast (edict^.eEntityState.esOrigin) Constants.multicastPvs