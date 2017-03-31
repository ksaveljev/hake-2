{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Game.GameTarget where

import Control.Lens (ix, (.=), zoom, (^.), (+=), (%=), use, preuse, (&), (.~), (+~), (%~))
import Control.Monad (liftM, when, void)
import Data.Bits ((.&.), (.|.), complement)
import Data.Char (toLower)
import Data.Maybe (isJust, fromJust, isNothing)
import Linear (V3(..), _x, _y, _z)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Types
import QuakeRef
import QuakeState
import CVarVariables
import Game.Adapters
import qualified Constants
import qualified Game.GameBase as GameBase
import qualified Game.GameCombat as GameCombat
import qualified Game.GameSpawn as GameSpawn
import qualified Game.GameUtil as GameUtil
import {-# SOURCE #-} qualified Game.GameWeapon as GameWeapon
import qualified Util.Lib as Lib

spTargetTempEntity :: Ref EdictT -> Quake ()
spTargetTempEntity edictRef =
    modifyRef edictRef (\v -> v & eUse .~ Just useTargetTEnt)

spTargetSpeaker :: Ref EdictT -> Quake ()
spTargetSpeaker edictRef = do
    noise <- use $ gameBaseGlobals.gbSpawnTemp.stNoise
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let dprintf = gameImport^.giDprintf
        soundIndex = gameImport^.giSoundIndex
        linkEntity = gameImport^.giLinkEntity

    edict <- readRef edictRef

    if isNothing noise
      then
        dprintf $ "target_speaker with no noise set at " `B.append` Lib.vtos (edict^.eEntityState.esOrigin) `B.append` "\n"

      else do
        let Just noiseStr = noise
            buffer = if ".wav" `B.isInfixOf` noiseStr
                       then noiseStr
                       else noiseStr `B.append` ".wav"

        noiseIndex <- soundIndex (Just buffer)
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
        linkEntity edictRef

{-
- QUAKED target_help (1 0 1) (-16 -16 -24) (16 16 24) help1 When fired, the
- "message" key becomes the current personal computer string, and the
- message light will be set on all clients status bars.
-}
spTargetHelp :: Ref EdictT -> Quake ()
spTargetHelp edictRef = do
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    if deathmatchValue /= 0 -- auto-remove for deathmatch
      then
        GameUtil.freeEdict edictRef

      else do
        edict <- readRef edictRef

        case edict^.eMessage of
          Nothing -> do
            dprintf <- use $ gameBaseGlobals.gbGameImport.giDprintf
            dprintf $ (edict^.eClassName) `B.append` " with no message at " `B.append` Lib.vtos (edict^.eEntityState.esOrigin) `B.append` "\n"
            GameUtil.freeEdict edictRef

          Just _ -> do
            modifyRef edictRef (\v -> v & eUse .~ Just useTargetHelp)

spTargetSecret :: Ref EdictT -> Quake ()
spTargetSecret edictRef = do
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    if deathmatchValue /= 0 -- auto-remove for deathmatch
      then
        GameUtil.freeEdict edictRef

      else do
        soundIndex <- use $ gameBaseGlobals.gbGameImport.giSoundIndex
        noise <- use $ gameBaseGlobals.gbSpawnTemp.stNoise

        when (isNothing noise) $
          gameBaseGlobals.gbSpawnTemp.stNoise .= Just "misc/secret.wav"

        noiseStr <- use $ gameBaseGlobals.gbSpawnTemp.stNoise
        noiseIndex <- soundIndex noiseStr

        modifyRef edictRef (\v -> v & eNoiseIndex .~ noiseIndex
                                       & eUse .~ Just useTargetSecret
                                       & eSvFlags .~ Constants.svfNoClient)

        gameBaseGlobals.gbLevel.llTotalSecrets += 1

        -- map bug hack
        edict <- readRef edictRef
        mapName <- liftM (BC.map toLower) (use $ gameBaseGlobals.gbLevel.llMapName)

        when ("mine3" == mapName && (edict^.eEntityState.esOrigin) == V3 280 (-2048) (-624)) $
          modifyRef edictRef (\v -> v & eMessage .~ Just "You have found a secret area.")

spTargetGoal :: Ref EdictT -> Quake ()
spTargetGoal edictRef = do
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    if deathmatchValue /= 0 -- auto-remove for deathmatch
      then 
        GameUtil.freeEdict edictRef

      else do
        soundIndex <- use $ gameBaseGlobals.gbGameImport.giSoundIndex

        modifyRef edictRef (\v -> v & eUse .~ Just useTargetGoal)

        noise <- (use $ gameBaseGlobals.gbSpawnTemp.stNoise) >>= \n -> if isJust n then return n else return (Just "misc/secret.wav")

        noiseIdx <- soundIndex noise
        modifyRef edictRef (\v -> v & eSvFlags .~ Constants.svfNoClient
                                       & eNoiseIndex .~ noiseIdx)

        gameBaseGlobals.gbLevel.llTotalGoals += 1

spTargetExplosion :: Ref EdictT -> Quake ()
spTargetExplosion edictRef = do
    modifyRef edictRef (\v -> v & eUse .~ Just useTargetExplosion
                                   & eSvFlags .~ Constants.svfNoClient)

spTargetChangeLevel :: Ref EdictT -> Quake ()
spTargetChangeLevel edictRef = do
    edict <- readRef edictRef

    case edict^.eMap of
      Nothing -> do
        dprintf <- use $ gameBaseGlobals.gbGameImport.giDprintf
        dprintf $ "target_changelevel with no map at " `B.append` Lib.vtos (edict^.eEntityState.esOrigin) `B.append` "\n"
        GameUtil.freeEdict edictRef

      Just _ -> do
        -- ugly hack because *SOMEBODY* screwed up their map
        mapName <- liftM (BC.map toLower) (use $ gameBaseGlobals.gbLevel.llMapName)
        when (mapName == "fact1" && BC.map toLower (fromJust $ edict^.eMap) == "fact3") $
          modifyRef edictRef (\v -> v & eMap .~ Just "fact3$secret1")

        modifyRef edictRef (\v -> v & eUse .~ Just useTargetChangeLevel
                                       & eSvFlags .~ Constants.svfNoClient)

spTargetSplash :: Ref EdictT -> Quake ()
spTargetSplash edictRef = do
    modifyRef edictRef (\v -> v & eUse .~ Just useTargetSplash)
    edict <- readRef edictRef

    GameBase.setMoveDir edictRef

    when ((edict^.eCount) == 0) $
      modifyRef edictRef (\v -> v & eCount .~ 32)

    modifyRef edictRef (\v -> v & eSvFlags .~ Constants.svfNoClient)

spTargetSpawner :: Ref EdictT -> Quake ()
spTargetSpawner selfRef = do
    modifyRef selfRef (\v -> v & eUse .~ Just useTargetSpawner
                                  & eSvFlags .~ Constants.svfNoClient)

    self <- readRef selfRef

    when ((self^.eSpeed) /= 0) $ do
      GameBase.setMoveDir selfRef
      modifyRef selfRef (\v -> v & eMoveDir %~ fmap (* (self^.eSpeed)))

spTargetBlaster :: Ref EdictT -> Quake ()
spTargetBlaster selfRef = do
    GameBase.setMoveDir selfRef

    soundIndex <- use $ gameBaseGlobals.gbGameImport.giSoundIndex
    soundIdx <- soundIndex (Just "weapons/laser2.wav")
    
    modifyRef selfRef (\v -> v & eUse .~ Just useTargetBlaster
                                  & eNoiseIndex .~ soundIdx
                                  & eDmg %~ (\d -> if d == 0 then 15 else d)
                                  & eSpeed %~ (\s -> if s == 0 then 1000 else s)
                                  & eSvFlags .~ Constants.svfNoClient)

spTargetCrossLevelTrigger :: Ref EdictT -> Quake ()
spTargetCrossLevelTrigger selfRef =
    modifyRef selfRef (\v -> v & eSvFlags .~ Constants.svfNoClient
                                  & eUse .~ Just triggerCrossLevelTriggerUse)

spTargetCrossLevelTarget :: Ref EdictT -> Quake ()
spTargetCrossLevelTarget selfRef = do
    self <- readRef selfRef
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    let delay = if (self^.eDelay) == 0 then 1 else self^.eDelay

    modifyRef selfRef (\v -> v & eDelay .~ delay
                                  & eSvFlags .~ Constants.svfNoClient
                                  & eThink .~ Just targetCrossLevelTargetThink
                                  & eNextThink .~ levelTime + delay)

spTargetLaser :: Ref EdictT -> Quake ()
spTargetLaser selfRef = do
    -- let everything else get spawned before we start firing
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime
    modifyRef selfRef (\v -> v & eThink .~ Just targetLaserStart
                                  & eNextThink .~ levelTime + 1)

spTargetLightRamp :: Ref EdictT -> Quake ()
spTargetLightRamp _ = io (putStrLn "GameTarget.spTargetLightRamp") >> undefined -- TODO

spTargetEarthquake :: Ref EdictT -> Quake ()
spTargetEarthquake selfRef = do
    self <- readRef selfRef
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let dprintf = gameImport^.giDprintf
        soundIndex = gameImport^.giSoundIndex

    when (isNothing (self^.eTargetName)) $
      dprintf ("untargeted " `B.append` (self^.eClassName) `B.append` " at " `B.append` Lib.vtos (self^.eEntityState.esOrigin) `B.append` "\n")

    noiseIndex <- soundIndex (Just "world/quake.wav")

    modifyRef selfRef (\v -> v & eCount %~ (\c -> if c == 0 then 5 else c)
                                  & eSpeed %~ (\s -> if s == 0 then 200 else s)
                                  & eSvFlags %~ (.|. Constants.svfNoClient)
                                  & eThink .~ Just targetEarthquakeThink
                                  & eUse .~ Just targetEarthquakeUse
                                  & eNoiseIndex .~ noiseIndex)

useTargetExplosion :: EntUse
useTargetExplosion =
  GenericEntUse "use_target_explosion" $ \selfRef _ activatorRef -> do
    modifyRef selfRef (\v -> v & eActivator .~ activatorRef)

    self <- readRef selfRef

    if (self^.eDelay) == 0
      then
        void $ think targetExplosionExplode selfRef

      else do
        levelTime <- use $ gameBaseGlobals.gbLevel.llTime

        modifyRef selfRef (\v -> v & eThink .~ Just targetExplosionExplode
                                      & eNextThink .~ levelTime + (self^.eDelay))

{-
- QUAKED target_goal (1 0 1) (-8 -8 -8) (8 8 8) Counts a goal completed.
- These are single use targets.
-}
useTargetGoal :: EntUse
useTargetGoal =
  GenericEntUse "use_target_goal" $ \edictRef _ activatorRef -> do
    edict <- readRef edictRef
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let sound = gameImport^.giSound
        configString = gameImport^.giConfigString

    sound (Just edictRef) Constants.chanVoice (edict^.eNoiseIndex) 1 Constants.attnNorm 0

    gameBaseGlobals.gbLevel.llFoundGoals += 1

    use (gameBaseGlobals.gbLevel) >>= \level ->
      when ((level^.llFoundGoals) == (level^.llTotalGoals)) $
        configString Constants.csCdTrack "0"

    GameUtil.useTargets edictRef activatorRef
    GameUtil.freeEdict edictRef

{-
- QUAKED target_speaker (1 0 0) (-8 -8 -8) (8 8 8) looped-on looped-off
- reliable "noise" wav file to play "attenuation" -1 = none, send to whole
- level 1 = normal fighting sounds 2 = idle sound level 3 = ambient sound
- level "volume" 0.0 to 1.0
- 
- Normal sounds play each time the target is used. The reliable flag can be
- set for crucial voiceovers.
- 
- Looped sounds are always atten 3 / vol 1, and the use function toggles it
- on/off. Multiple identical looping sounds will just increase volume
- without any speed cost.
-}
useTargetSpeaker :: EntUse
useTargetSpeaker =
  GenericEntUse "Use_Target_Speaker" $ \edictRef _ _ -> do
    edict <- readRef edictRef

    if (edict^.eSpawnFlags) .&. 3 /= 0 -- looping sound toggles
      then
        if (edict^.eEntityState.esSound) /= 0
          then modifyRef edictRef (\v -> v & eEntityState.esSound .~ 0) -- turn it off
          else modifyRef edictRef (\v -> v & eEntityState.esSound .~ (edict^.eNoiseIndex)) -- start it

      else do -- normal sound
        let chan = if (edict^.eSpawnFlags) .&. 4 /= 0
                     then Constants.chanVoice .|. Constants.chanReliable
                     else Constants.chanVoice

        -- use a positioned_sound, because this entity won't normally be
        -- sent to any clients because it is invisible
        positionedSound <- use $ gameBaseGlobals.gbGameImport.giPositionedSound
        positionedSound (Just $ edict^.eEntityState.esOrigin) edictRef chan (edict^.eNoiseIndex) (edict^.eVolume) (edict^.eAttenuation) 0

{-
- QUAKED target_secret (1 0 1) (-8 -8 -8) (8 8 8) Counts a secret found.
- These are single use targets.
-}
useTargetSecret :: EntUse
useTargetSecret =
  GenericEntUse "use_target_secret" $ \edictRef _ activatorRef -> do
    edict <- readRef edictRef
    sound <- use $ gameBaseGlobals.gbGameImport.giSound

    sound (Just edictRef) Constants.chanVoice (edict^.eNoiseIndex) 1 Constants.attnNorm 0

    gameBaseGlobals.gbLevel.llFoundSecrets += 1

    GameUtil.useTargets edictRef activatorRef
    GameUtil.freeEdict edictRef

useTargetHelp :: EntUse
useTargetHelp =
  GenericEntUse "Use_Target_Help" $ \edictRef _ _ -> do
    edict <- readRef edictRef

    if (edict^.eSpawnFlags) .&. 1 /= 0
      then gameBaseGlobals.gbGame.glHelpMessage1 .= fromJust (edict^.eMessage) -- TODO: are we sure about fromJust?
      else gameBaseGlobals.gbGame.glHelpMessage2 .= fromJust (edict^.eMessage) -- TODO: are we sure about fromJust?

    gameBaseGlobals.gbGame.glHelpChanged += 1

{-
- QUAKED target_splash (1 0 0) (-8 -8 -8) (8 8 8) Creates a particle splash
- effect when used.
- 
- Set "sounds" to one of the following: 1) sparks 2) blue water 3) brown
- water 4) slime 5) lava 6) blood
- 
- "count" how many pixels in the splash "dmg" if set, does a radius damage
- at this location when it splashes useful for lava/sparks
-}
useTargetSplash :: EntUse
useTargetSplash =
  GenericEntUse "use_target_splash" $ \selfRef _ activatorRef -> do
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let writeByte = gameImport^.giWriteByte
        writePosition = gameImport^.giWritePosition
        writeDir = gameImport^.giWriteDir
        multicast = gameImport^.giMulticast

    self <- readRef selfRef

    writeByte (fromIntegral Constants.svcTempEntity)
    writeByte (fromIntegral Constants.teSplash)
    writeByte (fromIntegral $ self^.eCount)
    writePosition (self^.eEntityState.esOrigin)
    writeDir (self^.eMoveDir)
    writeByte (fromIntegral $ self^.eSounds)
    multicast (self^.eEntityState.esOrigin) Constants.multicastPvs

    when ((self^.eDmg) /= 0) $
      GameCombat.radiusDamage selfRef (fromJust activatorRef) (fromIntegral $ self^.eDmg) Nothing (fromIntegral (self^.eDmg) + 40) Constants.modSplash

useTargetChangeLevel :: EntUse
useTargetChangeLevel =
  GenericEntUse "use_target_changelevel" $ \_ _ _ -> do
    io (putStrLn "GameTarget.useTargetChangeLevel") >> undefined -- TODO

targetExplosionExplode :: EntThink
targetExplosionExplode =
  GenericEntThink "target_explosion_explode" $ \selfRef -> do
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let writeByte = gameImport^.giWriteByte
        writePosition = gameImport^.giWritePosition
        multicast = gameImport^.giMulticast

    self <- readRef selfRef

    writeByte Constants.svcTempEntity
    writeByte Constants.teExplosion1
    writePosition (self^.eEntityState.esOrigin)
    multicast (self^.eEntityState.esOrigin) Constants.multicastPhs

    GameCombat.radiusDamage selfRef (fromJust $ self^.eActivator) (fromIntegral $ self^.eDmg) Nothing (fromIntegral (self^.eDmg) + 40) Constants.modExplosive

    let save = self^.eDelay

    modifyRef selfRef (\v -> v & eDelay .~ 0)
    GameUtil.useTargets selfRef (self^.eActivator)
    modifyRef selfRef (\v -> v & eDelay .~ save)

    return True

{-
- QUAKED target_temp_entity (1 0 0) (-8 -8 -8) (8 8 8) Fire an origin based
- temp entity event to the clients. "style" type byte
-}
useTargetTEnt :: EntUse
useTargetTEnt =
  GenericEntUse "Use_Target_Tent" $ \edictRef _ _ -> do
    edict <- readRef edictRef
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let writeByte = gameImport^.giWriteByte
        writePosition = gameImport^.giWritePosition
        multicast = gameImport^.giMulticast

    writeByte Constants.svcTempEntity
    writeByte (edict^.eStyle)
    writePosition (edict^.eEntityState.esOrigin)
    multicast (edict^.eEntityState.esOrigin) Constants.multicastPvs

{-
- QUAKED target_spawner (1 0 0) (-8 -8 -8) (8 8 8) Set target to the type
- of entity you want spawned. Useful for spawning monsters and gibs in the
- factory levels.
- 
- For monsters: Set direction to the facing you want it to have.
- 
- For gibs: Set direction if you want it moving and speed how fast it
- should be moving otherwise it will just be dropped
-}
useTargetSpawner :: EntUse
useTargetSpawner =
  GenericEntUse "use_target_spawner" $ \selfRef _ _ -> do
    self <- readRef selfRef
    edictRef <- GameUtil.spawn

    modifyRef edictRef (\v -> v & eClassName .~ fromJust (self^.eTarget)
                                   & eEntityState.esOrigin .~ (self^.eEntityState.esOrigin)
                                   & eEntityState.esAngles .~ (self^.eEntityState.esAngles))

    GameSpawn.callSpawn edictRef

    gameImport <- use $ gameBaseGlobals.gbGameImport

    let linkEntity = gameImport^.giLinkEntity
        unlinkEntity = gameImport^.giUnlinkEntity

    unlinkEntity edictRef
    GameUtil.killBox edictRef
    linkEntity edictRef

    self' <- readRef selfRef

    when ((self'^.eSpeed) /= 0) $
      modifyRef edictRef (\v -> v & eVelocity .~ (self'^.eMoveDir))

{-
- QUAKED target_blaster (1 0 0) (-8 -8 -8) (8 8 8) NOTRAIL NOEFFECTS Fires
- a blaster bolt in the set direction when triggered.
- 
- dmg default is 15 speed default is 1000
-}
useTargetBlaster :: EntUse
useTargetBlaster =
  GenericEntUse "use_target_blaster" $ \selfRef _ _ -> do
    self <- readRef selfRef

    let effect = if | (self^.eSpawnFlags) .&. 2 /= 0 -> 0
                    | (self^.eSpawnFlags) .&. 1 /= 0 -> Constants.efHyperblaster
                    | otherwise -> Constants.efBlaster
    
    GameWeapon.fireBlaster selfRef (self^.eEntityState.esOrigin) (self^.eMoveDir) (self^.eDmg) (truncate $ self^.eSpeed) Constants.efBlaster (Constants.modTargetBlaster /= 0) -- True

    sound <- use $ gameBaseGlobals.gbGameImport.giSound
    sound (Just selfRef) Constants.chanVoice (self^.eNoiseIndex) 1 Constants.attnNorm 0

{-
- QUAKED target_crosslevel_trigger (.5 .5 .5) (-8 -8 -8) (8 8 8) trigger1
- trigger2 trigger3 trigger4 trigger5 trigger6 trigger7 trigger8 Once this
- trigger is touched/used, any trigger_crosslevel_target with the same
- trigger number is automatically used when a level is started within the
- same unit. It is OK to check multiple triggers. Message, delay, target,
- and killtarget also work.
-}
triggerCrossLevelTriggerUse :: EntUse
triggerCrossLevelTriggerUse =
  GenericEntUse "trigger_crosslevel_trigger_use" $ \selfRef _ _ -> do
    self <- readRef selfRef
    gameBaseGlobals.gbGame.glServerFlags %= (.|. (self^.eSpawnFlags))
    GameUtil.freeEdict selfRef

{-
- QUAKED target_crosslevel_target (.5 .5 .5) (-8 -8 -8) (8 8 8) trigger1
- trigger2 trigger3 trigger4 trigger5 trigger6 trigger7 trigger8 Triggered
- by a trigger_crosslevel elsewhere within a unit. If multiple triggers are
- checked, all must be true. Delay, target and killtarget also work.
- 
- "delay" delay before using targets if the trigger has been activated
- (default 1)
-}
targetCrossLevelTargetThink :: EntThink
targetCrossLevelTargetThink =
  GenericEntThink "target_crosslevel_target_think" $ \selfRef -> do
    self <- readRef selfRef
    serverFlags <- use $ gameBaseGlobals.gbGame.glServerFlags

    when ((self^.eSpawnFlags) == (serverFlags .&. Constants.sflCrossTriggerMask .&. (self^.eSpawnFlags))) $ do
      GameUtil.useTargets selfRef (Just selfRef)
      GameUtil.freeEdict selfRef

    return True

targetLaserStart :: EntThink
targetLaserStart =
  GenericEntThink "target_laser_start" $ \selfRef -> do
    self <- readRef selfRef

        -- set the beam diameter
    let frame = if (self^.eSpawnFlags) .&. 64 /= 0
                  then 16
                  else 4
        -- set the color
        skinNum = if | (self^.eSpawnFlags) .&. 2 /= 0 -> 0xF2F2F0F0
                     | (self^.eSpawnFlags) .&. 4 /= 0 -> 0xD0D1D2D3
                     | (self^.eSpawnFlags) .&. 8 /= 0 -> 0xF3F3F1F1
                     | (self^.eSpawnFlags) .&. 16 /= 0 -> 0xDCDDDEDF
                     | (self^.eSpawnFlags) .&. 32 /= 0 -> 0xE0E1E2E3
                     | otherwise -> self^.eEntityState.esSkinNum

    modifyRef selfRef (\v -> v & eMoveType .~ Constants.moveTypeNone
                                  & eSolid .~ Constants.solidNot
                                  & eEntityState.esRenderFx %~ (.|. (Constants.rfBeam .|. Constants.rfTranslucent))
                                  & eEntityState.esModelIndex .~ 1 -- must be non-zero
                                  & eEntityState.esFrame .~ frame
                                  & eEntityState.esSkinNum .~ skinNum)

    when (isNothing (self^.eEnemy)) $
      case self^.eTarget of
        Just target -> do
          foundTarget <- GameBase.gFind Nothing GameBase.findByTarget target

          when (isNothing foundTarget) $ do
            dprintf <- use $ gameBaseGlobals.gbGameImport.giDprintf
            dprintf ((self^.eClassName) `B.append` " at " `B.append` Lib.vtos (self^.eEntityState.esOrigin) `B.append` ": " `B.append` target `B.append` " is a bad target\n")

          modifyRef selfRef (\v -> v & eEnemy .~ foundTarget)

        Nothing ->
          GameBase.setMoveDir selfRef

    modifyRef selfRef (\v -> v & eUse .~ Just targetLaserUse
                                  & eThink .~ Just targetLaserThink
                                  & eDmg %~ (\d -> if d == 0 then 1 else d)
                                  & eMins .~ V3 (-8) (-8) (-8)
                                  & eMaxs .~ V3 8 8 8)

    linkEntity <- use $ gameBaseGlobals.gbGameImport.giLinkEntity
    linkEntity selfRef

    self' <- readRef selfRef

    if (self'^.eSpawnFlags) .&. 1 /= 0
      then targetLaserOn selfRef
      else targetLaserOff selfRef

    return True

targetLaserOn :: Ref EdictT -> Quake ()
targetLaserOn selfRef = do
    self <- readRef selfRef

    let activator = case self^.eActivator of
                      Just _ -> self^.eActivator
                      Nothing -> Just selfRef

    modifyRef selfRef (\v -> v & eActivator .~ activator
                                  & eSpawnFlags %~ (.|. 0x80000001)
                                  & eSvFlags %~ (.&. (complement Constants.svfNoClient)))

    void $ think targetLaserThink selfRef

targetLaserOff :: Ref EdictT -> Quake ()
targetLaserOff selfRef =
    modifyRef selfRef (\v -> v & eSpawnFlags %~ (.&. (complement 1))
                                  & eSvFlags %~ (.|. Constants.svfNoClient)
                                  & eNextThink .~ 0)

targetLaserUse :: EntUse
targetLaserUse =
  GenericEntUse "target_laser_use" $ \selfRef _ activatorRef -> do
    modifyRef selfRef (\v -> v & eActivator .~ activatorRef)

    self <- readRef selfRef

    if (self^.eSpawnFlags) .&. 1 /= 0
      then targetLaserOff selfRef
      else targetLaserOn selfRef

{-
- QUAKED target_laser (0 .5 .8) (-8 -8 -8) (8 8 8) START_ON RED GREEN BLUE
- YELLOW ORANGE FAT When triggered, fires a laser. You can either set a
- target or a direction.
-}
targetLaserThink :: EntThink
targetLaserThink =
  GenericEntThink "target_laser_think" $ \selfRef -> do
    io (putStrLn "GameTarget.targetLaserThink") >> undefined -- TODO

{-
- QUAKED target_earthquake (1 0 0) (-8 -8 -8) (8 8 8) When triggered, this
- initiates a level-wide earthquake. All players and monsters are affected.
- "speed" severity of the quake (default:200) "count" duration of the quake
- (default:5)
-}
targetEarthquakeThink :: EntThink
targetEarthquakeThink =
  GenericEntThink "target_earthquake_think" $ \selfRef -> do
    self <- readRef selfRef
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    when ((self^.eLastMoveTime) < levelTime) $ do
      positionedSound <- use $ gameBaseGlobals.gbGameImport.giPositionedSound
      positionedSound (Just $ self^.eEntityState.esOrigin) selfRef Constants.chanAuto (self^.eNoiseIndex) 1.0 Constants.attnNone 0
      modifyRef selfRef (\v -> v & eLastMoveTime .~ levelTime + 0.5)

    numEdicts <- use $ gameBaseGlobals.gbNumEdicts

    shakeEdicts (self^.eSpeed) 1 numEdicts

    when (levelTime < (self^.eTimeStamp)) $
      modifyRef selfRef (\v -> v & eNextThink .~ levelTime + Constants.frameTime)

    return True

  where shakeEdicts :: Float -> Int -> Int -> Quake ()
        shakeEdicts speed idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              let edictRef = Ref idx

              edict <- readRef edictRef

              if not (edict^.eInUse) || isNothing (edict^.eClient) || isNothing (edict^.eGroundEntity)
                then
                  shakeEdicts speed (idx + 1) maxIdx

                else do
                  c1 <- Lib.crandom
                  c2 <- Lib.crandom
                  let velocity = (edict^.eVelocity) & _x +~ (c1 * 150)
                                                    & _y +~ (c2 * 150)
                                                    & _z .~ speed * (100.0 / fromIntegral (edict^.eMass))

                  modifyRef edictRef (\v -> v & eGroundEntity .~ Nothing
                                                 & eVelocity .~ velocity)

                  shakeEdicts speed (idx + 1) maxIdx

targetEarthquakeUse :: EntUse
targetEarthquakeUse =
  GenericEntUse "target_earthquake_use" $ \selfRef _ activatorRef -> do
    self <- readRef selfRef
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    modifyRef selfRef (\v -> v & eTimeStamp .~ levelTime + fromIntegral (self^.eCount)
                                  & eNextThink .~ levelTime + Constants.frameTime
                                  & eActivator .~ activatorRef
                                  & eLastMoveTime .~ 0)
