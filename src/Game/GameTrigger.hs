module Game.GameTrigger
    ( spTriggerAlways
    , spTriggerCounter
    , spTriggerGravity
    , spTriggerHurt
    , spTriggerKey
    , spTriggerMonsterJump
    , spTriggerMultiple
    , spTriggerOnce
    , spTriggerPush
    , spTriggerRelay
    ) where

import           Control.Lens      (use, (^.), (.=), (%=), (&), (.~), (%~), (-~))
import           Control.Monad     (when, unless)
import           Data.Bits         (complement, (.&.), (.|.))
import qualified Data.ByteString   as B
import           Linear            (_y, _z)

import qualified Constants
import qualified Game.GameBase     as GameBase
import           Game.EdictT
import           Game.EntityStateT
import qualified Game.GameUtil     as GameUtil
import           Game.LevelLocalsT
import           Game.SpawnTempT
import qualified QCommon.Com       as Com
import           QuakeRef
import           QuakeState
import           Types
import           Util.Binary       (encode)
import qualified Util.Lib          as Lib

spTriggerAlways :: Ref EdictT -> Quake ()
spTriggerAlways edictRef = do
    edict <- readRef edictRef
    when ((edict^.eDelay) < 0.2) $
        modifyRef edictRef (\v -> v & eDelay .~ 0.2)
    GameUtil.useTargets edictRef (Just edictRef)

spTriggerCounter :: Ref EdictT -> Quake ()
spTriggerCounter = error "GameTrigger.spTriggerCounter" -- TODO

spTriggerGravity :: Ref EdictT -> Quake ()
spTriggerGravity = error "GameTrigger.spTriggerGravity" -- TODO

spTriggerHurt :: Ref EdictT -> Quake ()
spTriggerHurt = error "GameTrigger.spTriggerHurt" -- TODO

spTriggerKey :: Ref EdictT -> Quake ()
spTriggerKey = error "GameTrigger.spTriggerKey" -- TODO

spTriggerMonsterJump :: Ref EdictT -> Quake ()
spTriggerMonsterJump = error "GameTrigger.spTriggerMonsterJump" -- TODO

spTriggerMultiple :: Ref EdictT -> Quake ()
spTriggerMultiple edictRef = do
    edict <- readRef edictRef
    gameImport <- use (gameBaseGlobals.gbGameImport)
    maybe (return ()) setNoiseIndex (getNoise edict)
    when ((edict^.eWait) == 0) $
        modifyRef edictRef (\v -> v & eWait .~ 0.2)
    modifyRef edictRef (\v -> v & eTouch .~ Just touchMulti
                                & eMoveType .~ Constants.moveTypeNone
                                & eSvFlags %~ (.|. Constants.svfNoClient))
    setSolidAndUse edict
    origin <- use (globals.gVec3Origin)
    unless ((edict^.eEntityState.esAngles) == origin) $
        GameBase.setMoveDir edictRef =<< readRef edictRef
    (gameImport^.giSetModel) edictRef (edict^.eiModel)
    (gameImport^.giLinkEntity) edictRef
  where
    getNoise edict
        | (edict^.eSounds) == 1 = Just "misc/secret.wav"
        | (edict^.eSounds) == 2 = Just "misc/talk.wav"
        | (edict^.eSounds) == 3 = Just "misc/trigger1.wav"
        | otherwise             = Nothing
    setNoiseIndex noise = do
        soundIndex <- use (gameBaseGlobals.gbGameImport.giSoundIndex)
        noiseIdx <- soundIndex (Just noise)
        modifyRef edictRef (\v -> v & eNoiseIndex .~ noiseIdx)
    setSolidAndUse edict
        | (edict^.eSpawnFlags) .&. 4 /= 0 =
            modifyRef edictRef (\v -> v & eSolid .~ Constants.solidNot
                                        & eUse .~ Just triggerEnable)
        | otherwise =
            modifyRef edictRef (\v -> v & eSolid .~ Constants.solidTrigger
                                        & eUse .~ Just useMulti)

triggerEnable :: EntUse
triggerEnable = EntUse "trigger_enable" $ \selfRef _ _ -> do
    modifyRef selfRef (\v -> v & eSolid .~ Constants.solidTrigger
                               & eUse .~ Just useMulti)
    linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
    linkEntity selfRef

useMulti :: EntUse
useMulti = EntUse "Use_Multi" $ \edictRef _ activatorRef -> do
    modifyRef edictRef (\v -> v & eActivator .~ activatorRef)
    multiTrigger edictRef

multiTrigger :: Ref EdictT -> Quake ()
multiTrigger = error "GameTrigger.multiTrigger" -- TODO

touchMulti :: EntTouch
touchMulti = error "GameTrigger.touchMulti" -- TODO

spTriggerOnce :: Ref EdictT -> Quake ()
spTriggerOnce edictRef = do
    edict <- readRef edictRef
    -- make old maps work because I messed up on flag assignments here
    -- triggered was on bit 1 when it should have been on bit 4
    when ((edict^.eSpawnFlags) .&. 1 /= 0) $ do
        let v = (edict^.eMins) + fmap (* 0.5) (edict^.eSize)
        modifyRef edictRef (\v -> v & eSpawnFlags %~ (.&. (complement 1))
                                    & eSpawnFlags %~ (.|. 4))
        dprintf <- use (gameBaseGlobals.gbGameImport.giDprintf)
        dprintf (B.concat ["fixed TRIGGERED flag on ", edict^.eClassName, Lib.vtos v, "\n"])
    modifyRef edictRef (\v -> v & eWait .~ (-1))
    spTriggerMultiple edictRef

spTriggerPush :: Ref EdictT -> Quake ()
spTriggerPush = error "GameTrigger.spTriggerPush" -- TODO

spTriggerRelay :: Ref EdictT -> Quake ()
spTriggerRelay edictRef =
    modifyRef edictRef (\v -> v & eUse .~ Just triggerRelayUse)

triggerRelayUse :: EntUse
triggerRelayUse = EntUse "trigger_relay_use" $ \selfRef _ activatorRef -> do
    GameUtil.useTargets selfRef activatorRef
