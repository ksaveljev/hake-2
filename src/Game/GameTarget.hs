{-# LANGUAGE OverloadedStrings #-}
module Game.GameTarget where

import Control.Lens (ix, (.=), zoom, (^.), (+=), use, preuse, (+=))
import Control.Monad (liftM, when)
import Data.Bits ((.&.))
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
import qualified Game.GameUtil as GameUtil
import qualified Util.Lib as Lib

spTargetTempEntity :: EdictReference -> Quake ()
spTargetTempEntity _ = io (putStrLn "GameTarget.spTargetTempEntity") >> undefined -- TODO

spTargetSpeaker :: EdictReference -> Quake ()
spTargetSpeaker er@(EdictReference edictIdx) = do
    noise <- use $ gameBaseGlobals.gbSpawnTemp.stNoise
    gameImport <- use $ gameBaseGlobals.gbGameImport
    let dprintf = gameImport^.giDprintf
        soundIndex = gameImport^.giSoundIndex
        linkEntity = gameImport^.giLinkEntity

    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

    if isNothing noise
      then dprintf $ "target_speaker with no noise set at " `B.append` Lib.vtos (edict^.eEntityState.esOrigin) `B.append` "\n"
      else do
        let Just noiseStr = noise
            buffer = if ".wav" `B.isInfixOf` noiseStr
                       then noiseStr
                       else noiseStr `B.append` ".wav"

        noiseIndex <- soundIndex buffer
        gameBaseGlobals.gbGEdicts.ix edictIdx.eNoiseIndex .= noiseIndex

        when ((edict^.eVolume) == 0) $
          gameBaseGlobals.gbGEdicts.ix edictIdx.eVolume .= 1
        
        when ((edict^.eAttenuation) == 0) $
          gameBaseGlobals.gbGEdicts.ix edictIdx.eAttenuation .= 1
        when ((edict^.eAttenuation) == (-1)) $
          gameBaseGlobals.gbGEdicts.ix edictIdx.eAttenuation .= 0

        -- check for prestarted looping sound
        when ((edict^.eSpawnFlags) .&. 1 /= 0) $
          gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esSound .= noiseIndex

        gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictAction.eaUse .= Just useTargetSpeaker

        -- must link the entity so we get areas and clusters so
        -- the server can determine who to send updates to
        linkEntity er

spTargetHelp :: EdictReference -> Quake ()
spTargetHelp _ = io (putStrLn "GameTarget.spTargetHelp") >> undefined -- TODO

spTargetSecret :: EdictReference -> Quake ()
spTargetSecret er@(EdictReference edictIdx) = do
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    if deathmatchValue /= 0 -- auto-remove for deathmatch
      then GameUtil.freeEdict er
      else do
        soundIndex <- use $ gameBaseGlobals.gbGameImport.giSoundIndex
        noise <- use $ gameBaseGlobals.gbSpawnTemp.stNoise

        when (isNothing noise) $
          gameBaseGlobals.gbSpawnTemp.stNoise .= Just "misc/secret.wav"

        noiseStr <- liftM fromJust (use $ gameBaseGlobals.gbSpawnTemp.stNoise)
        noiseIndex <- soundIndex noiseStr

        zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
          eNoiseIndex .= noiseIndex
          eEdictAction.eaUse .= Just useTargetSecret
          eSvFlags .= Constants.svfNoClient

        gameBaseGlobals.gbLevel.llTotalSecrets += 1


        io (putStrLn "GameTarget.spTargetSecret") >> undefined -- TODO

spTargetGoal :: EdictReference -> Quake ()
spTargetGoal er@(EdictReference edictIdx) = do
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    if deathmatchValue /= 0 -- auto-remove for deathmatch
      then GameUtil.freeEdict er
      else do
        soundIndex <- use $ gameBaseGlobals.gbGameImport.giSoundIndex

        gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictAction.eaUse .= Just useTargetGoal

        noise <- (use $ gameBaseGlobals.gbSpawnTemp.stNoise) >>= \n -> if isJust n then return (fromJust n) else return "misc/secret.wav"

        soundIndex noise >>= (gameBaseGlobals.gbGEdicts.ix edictIdx.eNoiseIndex .=)
        gameBaseGlobals.gbGEdicts.ix edictIdx.eSvFlags .= Constants.svfNoClient
        gameBaseGlobals.gbLevel.llTotalGoals += 1

        -- map bug hack
        Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
        mapName <- liftM (BC.map toLower) (use $ gameBaseGlobals.gbLevel.llMapName)

        when ("mine3" == mapName && (edict^.eEntityState.esOrigin) == V3 280 (-2048) (-624)) $
          gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictInfo.eiMessage .= Just "You have found a secret area."

spTargetExplosion :: EdictReference -> Quake ()
spTargetExplosion (EdictReference edictIdx) = do
    zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
      eEdictAction.eaUse .= Just useTargetExplosion
      eSvFlags .= Constants.svfNoClient

spTargetChangeLevel :: EdictReference -> Quake ()
spTargetChangeLevel _ = io (putStrLn "GameTarget.spTargetChangeLevel") >> undefined -- TODO

spTargetSplash :: EdictReference -> Quake ()
spTargetSplash _ = io (putStrLn "GameTarget.spTargetSplash") >> undefined -- TODO

spTargetSpawner :: EdictReference -> Quake ()
spTargetSpawner _ = io (putStrLn "GameTarget.spTargetSpawner") >> undefined -- TODO

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
  GenericEntUse "use_target_explosion" $ \_ _ _ -> do
    io (putStrLn "GameTarget.useTargetExplosion") >> undefined -- TODO

useTargetGoal :: EntUse
useTargetGoal =
  GenericEntUse "use_target_goal" $ \_ _ _ -> do
    io (putStrLn "GameTarget.useTargetGoal") >> undefined -- TODO

useTargetSpeaker :: EntUse
useTargetSpeaker =
  GenericEntUse "Use_Target_Speaker" $ \_ _ _ -> do
    io (putStrLn "GameTarget.useTargetSpeaker") >> undefined -- TODO

useTargetSecret :: EntUse
useTargetSecret =
  GenericEntUse "use_target_secret" $ \_ _ _ -> do
    io (putStrLn "GameTarget.useTargetSecret") >> undefined -- TODO
