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

spTriggerAlways :: Ref' EdictT -> Quake ()
spTriggerAlways edictRef = do
    edict <- readRef edictRef
    when ((edict^.eDelay) < 0.2) $
        modifyRef edictRef (\v -> v & eDelay .~ 0.2)
    GameUtil.useTargets edictRef (Just edictRef)

spTriggerCounter :: Ref' EdictT -> Quake ()
spTriggerCounter selfRef = do
    modifyRef selfRef (\v -> v & eWait -~ 1
                               & eCount %~ (\a -> if a == 0 then 2 else a)
                               & eUse .~ Just triggerCounterUse)

triggerCounterUse :: EntUse
triggerCounterUse = EntUse "trigger_counter_use" $ \selfRef _ mActivatorRef -> do
    self <- readRef selfRef
    unless ((self^.eCount) == 0) $ do
        modifyRef selfRef (\v -> v & eCount .~ (self^.eCount) - 1)
        gameImport <- use (gameBaseGlobals.gbGameImport)
        updateSequence selfRef self mActivatorRef gameImport ((self^.eCount) - 1)
  where
    updateSequence _ _ Nothing _ _ =
        Com.fatalError "GameTrigger.triggerCounterUse mActivatorRef is Nothing"
    updateSequence selfRef self mActivatorRef@(Just activatorRef) gameImport count
        | count > 0 = do
            when ((self^.eSpawnFlags) .&. 1 == 0) $ do
                (gameImport^.giCenterPrintf) activatorRef (B.concat [encode count, " more to go..."])
                soundIdx <- (gameImport^.giSoundIndex) (Just "misc/talk1.wav")
                (gameImport^.giSound) mActivatorRef Constants.chanAuto soundIdx 1 Constants.attnNorm 0
        | otherwise = do
            when ((self^.eSpawnFlags) .&. 1 == 0) $ do
                (gameImport^.giCenterPrintf) activatorRef "Sequence completed!"
                soundIdx <- (gameImport^.giSoundIndex) (Just "misc/talk1.wav")
                (gameImport^.giSound) mActivatorRef Constants.chanAuto soundIdx 1 Constants.attnNorm 0
            modifyRef selfRef (\v -> v & eActivator .~ mActivatorRef)
            multiTrigger selfRef

spTriggerGravity :: Ref' EdictT -> Quake ()
spTriggerGravity selfRef = do
    mGravity <- use (gameBaseGlobals.gbSpawnTemp.stGravity)
    maybe nothingToTrigger triggerGravity mGravity
  where
    nothingToTrigger = do
        self <- readRef selfRef
        dprintf <- use (gameBaseGlobals.gbGameImport.giDprintf)
        dprintf (B.concat ["trigger_gravity without gravity set at ", Lib.vtos (self^.eEntityState.esOrigin), "\n"])
        GameUtil.freeEdict selfRef
    triggerGravity gravity = do
        initTrigger selfRef
        modifyRef selfRef (\v -> v & eGravity .~ fromIntegral (Lib.atoi gravity)
                                   & eTouch .~ Just triggerGravityTouch)

spTriggerHurt :: Ref' EdictT -> Quake ()
spTriggerHurt selfRef = do
    initTrigger selfRef
    self <- readRef selfRef
    soundIndex <- use (gameBaseGlobals.gbGameImport.giSoundIndex)
    soundIdx <- soundIndex (Just "world/electro.wav")
    modifyRef selfRef (\v -> v & eNoiseIndex .~ soundIdx
                               & eTouch .~ Just hurtTouch
                               & eDmg %~ (\a -> if a == 0 then 5 else a)
                               & eSolid .~ getSolid self
                               & eUse .~ getSelfUse self)
    linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
    linkEntity selfRef
  where
    getSolid self
        | (self^.eSpawnFlags) .&. 1 /= 0 = Constants.solidNot
        | otherwise                      = Constants.solidTrigger
    getSelfUse self
        | (self^.eSpawnFlags) .&. 2 /= 0 = Just hurtUse
        | otherwise                      = self^.eUse

spTriggerKey :: Ref' EdictT -> Quake ()
spTriggerKey = error "GameTrigger.spTriggerKey" -- TODO

spTriggerMonsterJump :: Ref' EdictT -> Quake ()
spTriggerMonsterJump selfRef = do
    modifyRef selfRef (\v -> v & eSpeed %~ (\a -> if a == 0 then 200 else a)
                               & eEntityState.esAngles._y %~ (\a -> if a == 0 then 360 else a)) -- IMPROVE: use Constants.yaw instead of using _y directly
    gameBaseGlobals.gbSpawnTemp.stHeight %= (\v -> if v == 0 then 200 else v)
    height <- use (gameBaseGlobals.gbSpawnTemp.stHeight)
    initTrigger selfRef
    modifyRef selfRef (\v -> v & eTouch .~ Just triggerMonsterJumpTouch
                               & eMoveDir._z .~ fromIntegral height)

spTriggerMultiple :: Ref' EdictT -> Quake ()
spTriggerMultiple = error "GameTrigger.spTriggerMultiple" -- TODO

spTriggerOnce :: Ref' EdictT -> Quake ()
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

spTriggerPush :: Ref' EdictT -> Quake ()
spTriggerPush selfRef = do
    initTrigger selfRef
    soundIndex <- use (gameBaseGlobals.gbGameImport.giSoundIndex)
    windSound <- soundIndex (Just "misc/windfly.wav")
    gameBaseGlobals.gbWindSound .= windSound
    modifyRef selfRef (\v -> v & eTouch .~ Just triggerPushTouch
                               & eSpeed %~ (\a -> if a == 0 then 1000 else a))
    linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
    linkEntity selfRef

spTriggerRelay :: Ref' EdictT -> Quake ()
spTriggerRelay edictRef =
    modifyRef edictRef (\v -> v & eUse .~ Just triggerRelayUse)

multiTrigger :: Ref' EdictT -> Quake ()
multiTrigger edictRef = do
    edict <- readRef edictRef
    when ((edict^.eNextThink) == 0) $ do
      GameUtil.useTargets edictRef (edict^.eActivator)
      levelTime <- use (gameBaseGlobals.gbLevel.llTime)
      updateNextThink levelTime =<< readRef edictRef
  where
    updateNextThink levelTime edict
        | (edict^.eWait) > 0 =
            modifyRef edictRef (\v -> v & eThink .~ Just multiWait
                                        & eNextThink .~ levelTime + (edict^.eWait))
        | otherwise =
            -- we can't just remove (self) here, because this is a touch
            -- function called while looping through area links...
            modifyRef edictRef (\v -> v & eTouch .~ Nothing
                                        & eNextThink .~ levelTime + (Constants.frameTime)
                                        & eThink .~ Just GameUtil.freeEdictA)

multiWait :: EntThink
multiWait = EntThink "multi_wait" $ \edictRef -> do
    modifyRef edictRef (\v -> v & eNextThink .~ 0)
    return True

initTrigger :: Ref' EdictT -> Quake ()
initTrigger selfRef = do
    v3o <- use (globals.gVec3Origin)
    self <- readRef selfRef
    unless ((self^.eEntityState.esAngles) == v3o) $
        GameBase.setMoveDir selfRef =<< readRef selfRef
    modifyRef selfRef (\v -> v & eSolid .~ Constants.solidTrigger
                               & eMoveType .~ Constants.moveTypeNone
                               & eSvFlags .~ Constants.svfNoClient)
    setModel <- use (gameBaseGlobals.gbGameImport.giSetModel)
    setModel selfRef (self^.eiModel)

triggerGravityTouch :: EntTouch
triggerGravityTouch = EntTouch "trigger_gravity_touch" $ \selfRef otherRef _ _ -> do
    self <- readRef selfRef
    modifyRef otherRef (\v -> v & eGravity .~ (self^.eGravity))

hurtTouch :: EntTouch
hurtTouch = EntTouch "hurt_touch" $ \selfRef otherRef _ _ -> do
    error "GameTrigger.hurtTouch" -- TODO

hurtUse :: EntUse
hurtUse = EntUse "hurt_use" $ \selfRef _ _ -> do
    self <- readRef selfRef
    modifyRef selfRef (\v -> v & eSolid .~ getSolid self)
    linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
    linkEntity selfRef
    updateSelfUse selfRef
  where
    getSolid self
        | (self^.eSolid) == Constants.solidNot = Constants.solidTrigger
        | otherwise                            = Constants.solidNot
    updateSelfUse selfRef = do
        self <- readRef selfRef
        when ((self^.eSpawnFlags) .&. 2 == 0) $
            modifyRef selfRef (\v -> v & eUse .~ Nothing)

triggerMonsterJumpTouch :: EntTouch
triggerMonsterJumpTouch = EntTouch "trigger_monsterjump_touch" $ \selfRef otherRef _ _ -> do
    error "GameTrigger.triggerMonsterJumpTouch" -- TODO

triggerPushTouch :: EntTouch
triggerPushTouch = EntTouch "trigger_push_touch" $ \selfRef otherRef _ _ -> do
    error "GameTrigger.triggerPushTouch" -- TODO

triggerRelayUse :: EntUse
triggerRelayUse = EntUse "trigger_relay_use" $ \selfRef _ activatorRef -> do
    GameUtil.useTargets selfRef activatorRef