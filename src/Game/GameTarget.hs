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
spTargetBlaster = error "GameTarget.spTargetBlaster" -- TODO

spTargetChangeLevel :: Ref EdictT -> Quake ()
spTargetChangeLevel = error "GameTarget.spTargetChangeLevel" -- TODO

spTargetCrossLevelTarget :: Ref EdictT -> Quake ()
spTargetCrossLevelTarget = error "GameTarget.spTargetCrossLevelTarget" -- TODO

spTargetCrossLevelTrigger :: Ref EdictT -> Quake ()
spTargetCrossLevelTrigger = error "GameTarget.spTargetCrossLevelTrigger" -- TODO

spTargetEarthquake :: Ref EdictT -> Quake ()
spTargetEarthquake = error "GameTarget.spTargetEarthquake" -- TODO

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
spTargetGoal = error "GameTarget.spTargetGoal" -- TODO

spTargetHelp :: Ref EdictT -> Quake ()
spTargetHelp = error "GameTarget.spTargetHelp" -- TODO

spTargetLaser :: Ref EdictT -> Quake ()
spTargetLaser = error "GameTarget.spTargetLaser" -- TODO

spTargetLightRamp :: Ref EdictT -> Quake ()
spTargetLightRamp = error "GameTarget.spTargetLightRamp" -- TODO

spTargetSecret :: Ref EdictT -> Quake ()
spTargetSecret = error "GameTarget.spTargetSecret" -- TODO

spTargetSpawner :: Ref EdictT -> Quake ()
spTargetSpawner = error "GameTarget.spTargetSpawner" -- TODO

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
useTargetSpeaker = error "GameTarget.useTargetSpeaker" -- TODO

spTargetSplash :: Ref EdictT -> Quake ()
spTargetSplash = error "GameTarget.spTargetSplash" -- TODO

spTargetTempEntity :: Ref EdictT -> Quake ()
spTargetTempEntity = error "GameTarget.spTargetTempEntity" -- TODO
