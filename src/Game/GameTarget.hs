{-# LANGUAGE OverloadedStrings #-}
module Game.GameTarget where

import Control.Lens (ix, (.=), zoom, (^.), (+=), use, preuse, (&), (.~), (+~), (%~))
import Control.Monad (liftM, when, void)
import Data.Bits ((.&.), (.|.))
import Data.Char (toLower)
import Data.Maybe (isJust, fromJust, isNothing)
import Linear (V3(..))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Quake
import QuakeState
import CVarVariables
import Game.Adapters
import qualified Constants
import qualified Game.GameBase as GameBase
import qualified Game.GameCombat as GameCombat
import qualified Game.GameSpawn as GameSpawn
import qualified Game.GameUtil as GameUtil
import qualified Util.Lib as Lib

spTargetTempEntity :: EdictReference -> Quake ()
spTargetTempEntity edictRef =
    modifyEdictT edictRef (\v -> v & eUse .~ Just useTargetTEnt)

spTargetSpeaker :: EdictReference -> Quake ()
spTargetSpeaker edictRef = do
    noise <- use $ gameBaseGlobals.gbSpawnTemp.stNoise
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let dprintf = gameImport^.giDprintf
        soundIndex = gameImport^.giSoundIndex
        linkEntity = gameImport^.giLinkEntity

    edict <- readEdictT edictRef

    if isNothing noise
      then
        dprintf $ "target_speaker with no noise set at " `B.append` Lib.vtos (edict^.eEntityState.esOrigin) `B.append` "\n"

      else do
        let Just noiseStr = noise
            buffer = if ".wav" `B.isInfixOf` noiseStr
                       then noiseStr
                       else noiseStr `B.append` ".wav"

        noiseIndex <- soundIndex (Just buffer)
        modifyEdictT edictRef (\v -> v & eNoiseIndex .~ noiseIndex)

        when ((edict^.eVolume) == 0) $
          modifyEdictT edictRef (\v -> v & eVolume .~ 1)
        
        when ((edict^.eAttenuation) == 0) $
          modifyEdictT edictRef (\v -> v & eAttenuation .~ 1)
        when ((edict^.eAttenuation) == (-1)) $
          modifyEdictT edictRef (\v -> v & eAttenuation .~ 0)

        -- check for prestarted looping sound
        when ((edict^.eSpawnFlags) .&. 1 /= 0) $
          modifyEdictT edictRef (\v -> v & eEntityState.esSound .~ noiseIndex)

        modifyEdictT edictRef (\v -> v & eUse .~ Just useTargetSpeaker)

        -- must link the entity so we get areas and clusters so
        -- the server can determine who to send updates to
        linkEntity edictRef

{-
- QUAKED target_help (1 0 1) (-16 -16 -24) (16 16 24) help1 When fired, the
- "message" key becomes the current personal computer string, and the
- message light will be set on all clients status bars.
-}
spTargetHelp :: EdictReference -> Quake ()
spTargetHelp edictRef = do
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    if deathmatchValue /= 0 -- auto-remove for deathmatch
      then
        GameUtil.freeEdict edictRef

      else do
        edict <- readEdictT edictRef

        case edict^.eMessage of
          Nothing -> do
            dprintf <- use $ gameBaseGlobals.gbGameImport.giDprintf
            dprintf $ (edict^.eClassName) `B.append` " with no message at " `B.append` Lib.vtos (edict^.eEntityState.esOrigin) `B.append` "\n"
            GameUtil.freeEdict edictRef

          Just _ -> do
            modifyEdictT edictRef (\v -> v & eUse .~ Just useTargetHelp)

spTargetSecret :: EdictReference -> Quake ()
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

        modifyEdictT edictRef (\v -> v & eNoiseIndex .~ noiseIndex
                                       & eUse .~ Just useTargetSecret
                                       & eSvFlags .~ Constants.svfNoClient)

        gameBaseGlobals.gbLevel.llTotalSecrets += 1

        -- map bug hack
        edict <- readEdictT edictRef
        mapName <- liftM (BC.map toLower) (use $ gameBaseGlobals.gbLevel.llMapName)

        when ("mine3" == mapName && (edict^.eEntityState.esOrigin) == V3 280 (-2048) (-624)) $
          modifyEdictT edictRef (\v -> v & eMessage .~ Just "You have found a secret area.")

spTargetGoal :: EdictReference -> Quake ()
spTargetGoal edictRef = do
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    if deathmatchValue /= 0 -- auto-remove for deathmatch
      then 
        GameUtil.freeEdict edictRef

      else do
        soundIndex <- use $ gameBaseGlobals.gbGameImport.giSoundIndex

        modifyEdictT edictRef (\v -> v & eUse .~ Just useTargetGoal)

        noise <- (use $ gameBaseGlobals.gbSpawnTemp.stNoise) >>= \n -> if isJust n then return n else return (Just "misc/secret.wav")

        noiseIdx <- soundIndex noise
        modifyEdictT edictRef (\v -> v & eSvFlags .~ Constants.svfNoClient
                                       & eNoiseIndex .~ noiseIdx)

        gameBaseGlobals.gbLevel.llTotalGoals += 1

spTargetExplosion :: EdictReference -> Quake ()
spTargetExplosion edictRef = do
    modifyEdictT edictRef (\v -> v & eUse .~ Just useTargetExplosion
                                   & eSvFlags .~ Constants.svfNoClient)

spTargetChangeLevel :: EdictReference -> Quake ()
spTargetChangeLevel edictRef = do
    edict <- readEdictT edictRef

    case edict^.eMap of
      Nothing -> do
        dprintf <- use $ gameBaseGlobals.gbGameImport.giDprintf
        dprintf $ "target_changelevel with no map at " `B.append` Lib.vtos (edict^.eEntityState.esOrigin) `B.append` "\n"
        GameUtil.freeEdict edictRef

      Just _ -> do
        -- ugly hack because *SOMEBODY* screwed up their map
        mapName <- liftM (BC.map toLower) (use $ gameBaseGlobals.gbLevel.llMapName)
        when (mapName == "fact1" && BC.map toLower (fromJust $ edict^.eMap) == "fact3") $
          modifyEdictT edictRef (\v -> v & eMap .~ Just "fact3$secret1")

        modifyEdictT edictRef (\v -> v & eUse .~ Just useTargetChangeLevel
                                       & eSvFlags .~ Constants.svfNoClient)

spTargetSplash :: EdictReference -> Quake ()
spTargetSplash edictRef = do
    modifyEdictT edictRef (\v -> v & eUse .~ Just useTargetSplash)
    edict <- readEdictT edictRef

    GameBase.setMoveDir edictRef

    when ((edict^.eCount) == 0) $
      modifyEdictT edictRef (\v -> v & eCount .~ 32)

    modifyEdictT edictRef (\v -> v & eSvFlags .~ Constants.svfNoClient)

spTargetSpawner :: EdictReference -> Quake ()
spTargetSpawner selfRef = do
    modifyEdictT selfRef (\v -> v & eUse .~ Just useTargetSpawner
                                  & eSvFlags .~ Constants.svfNoClient)

    self <- readEdictT selfRef

    when ((self^.eSpeed) /= 0) $ do
      GameBase.setMoveDir selfRef
      modifyEdictT selfRef (\v -> v & eMoveDir %~ fmap (* (self^.eSpeed)))

spTargetBlaster :: EdictReference -> Quake ()
spTargetBlaster _ = io (putStrLn "GameTarget.spTargetBlaster") >> undefined -- TODO

spTargetCrossLevelTrigger :: EdictReference -> Quake ()
spTargetCrossLevelTrigger _ = io (putStrLn "GameTarget.spTargetCrossLevelTrigger") >> undefined -- TODO

spTargetCrossLevelTarget :: EdictReference -> Quake ()
spTargetCrossLevelTarget _ = io (putStrLn "GameTarget.spTargetCrossLevelTarget") >> undefined -- TODO

spTargetLaser :: EdictReference -> Quake ()
spTargetLaser _ = io (putStrLn "GameTarget.spTargetLaser") >> undefined -- TODO

spTargetLightRamp :: EdictReference -> Quake ()
spTargetLightRamp _ = io (putStrLn "GameTarget.spTargetLightRamp") >> undefined -- TODO

spTargetEarthquake :: EdictReference -> Quake ()
spTargetEarthquake _ = io (putStrLn "GameTarget.spTargetEarthquake") >> undefined -- TODO

useTargetExplosion :: EntUse
useTargetExplosion =
  GenericEntUse "use_target_explosion" $ \selfRef _ activatorRef -> do
    modifyEdictT selfRef (\v -> v & eActivator .~ activatorRef)

    self <- readEdictT selfRef

    if (self^.eDelay) == 0
      then
        void $ think targetExplosionExplode selfRef

      else do
        levelTime <- use $ gameBaseGlobals.gbLevel.llTime

        modifyEdictT selfRef (\v -> v & eThink .~ Just targetExplosionExplode
                                      & eNextThink .~ levelTime + (self^.eDelay))

useTargetGoal :: EntUse
useTargetGoal =
  GenericEntUse "use_target_goal" $ \_ _ _ -> do
    io (putStrLn "GameTarget.useTargetGoal") >> undefined -- TODO

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
    edict <- readEdictT edictRef

    if (edict^.eSpawnFlags) .&. 3 /= 0 -- looping sound toggles
      then
        if (edict^.eEntityState.esSound) /= 0
          then modifyEdictT edictRef (\v -> v & eEntityState.esSound .~ 0) -- turn it off
          else modifyEdictT edictRef (\v -> v & eEntityState.esSound .~ (edict^.eNoiseIndex)) -- start it

      else do -- normal sound
        let chan = if (edict^.eSpawnFlags) .&. 4 /= 0
                     then Constants.chanVoice .|. Constants.chanReliable
                     else Constants.chanVoice

        -- use a positioned_sound, because this entity won't normally be
        -- sent to any clients because it is invisible
        positionedSound <- use $ gameBaseGlobals.gbGameImport.giPositionedSound
        positionedSound (Just $ edict^.eEntityState.esOrigin) edictRef chan (edict^.eNoiseIndex) (edict^.eVolume) (edict^.eAttenuation) 0

useTargetSecret :: EntUse
useTargetSecret =
  GenericEntUse "use_target_secret" $ \_ _ _ -> do
    io (putStrLn "GameTarget.useTargetSecret") >> undefined -- TODO

useTargetHelp :: EntUse
useTargetHelp =
  GenericEntUse "Use_Target_Help" $ \edictRef _ _ -> do
    edict <- readEdictT edictRef

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

    self <- readEdictT selfRef

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

    self <- readEdictT selfRef

    writeByte Constants.svcTempEntity
    writeByte Constants.teExplosion1
    writePosition (self^.eEntityState.esOrigin)
    multicast (self^.eEntityState.esOrigin) Constants.multicastPhs

    GameCombat.radiusDamage selfRef (fromJust $ self^.eActivator) (fromIntegral $ self^.eDmg) Nothing (fromIntegral (self^.eDmg) + 40) Constants.modExplosive

    let save = self^.eDelay

    modifyEdictT selfRef (\v -> v & eDelay .~ 0)
    GameUtil.useTargets selfRef (self^.eActivator)
    modifyEdictT selfRef (\v -> v & eDelay .~ save)

    return True

{-
- QUAKED target_temp_entity (1 0 0) (-8 -8 -8) (8 8 8) Fire an origin based
- temp entity event to the clients. "style" type byte
-}
useTargetTEnt :: EntUse
useTargetTEnt =
  GenericEntUse "Use_Target_Tent" $ \edictRef _ _ -> do
    edict <- readEdictT edictRef
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
    self <- readEdictT selfRef
    edictRef <- GameUtil.spawn

    modifyEdictT edictRef (\v -> v & eClassName .~ fromJust (self^.eTarget)
                                   & eEntityState.esOrigin .~ (self^.eEntityState.esOrigin)
                                   & eEntityState.esAngles .~ (self^.eEntityState.esAngles))

    GameSpawn.callSpawn edictRef

    gameImport <- use $ gameBaseGlobals.gbGameImport

    let linkEntity = gameImport^.giLinkEntity
        unlinkEntity = gameImport^.giUnlinkEntity

    unlinkEntity edictRef

    GameUtil.killBox edictRef

    linkEntity edictRef

    self' <- readEdictT selfRef

    when ((self'^.eSpeed) /= 0) $
      modifyEdictT edictRef (\v -> v & eVelocity .~ (self'^.eMoveDir))
