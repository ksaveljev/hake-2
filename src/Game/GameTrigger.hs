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
import           Control.Monad     (when, unless, void)
import           Data.Bits         (complement, (.&.), (.|.))
import qualified Data.ByteString   as B
import           Data.Maybe        (isJust)
import           Linear            (dot, _x, _y, _z)

import qualified Constants
import qualified Game.GameBase     as GameBase
import qualified Game.GameCombat   as GameCombat
import qualified Game.GameItems    as GameItems
import           Game.GClientT
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
import qualified Util.Math3D       as Math3D

pushOnce :: Int
pushOnce = 1

spTriggerAlways :: Ref EdictT -> Quake ()
spTriggerAlways edictRef = do
    edict <- readRef edictRef
    when ((edict^.eDelay) < 0.2) $
        modifyRef edictRef (\v -> v & eDelay .~ 0.2)
    GameUtil.useTargets edictRef (Just edictRef)

spTriggerCounter :: Ref EdictT -> Quake ()
spTriggerCounter selfRef = do
    modifyRef selfRef (\v -> v & eWait -~ 1
                               & eCount %~ (\a -> if a == 0 then 2 else a)
                               & eUse .~ Just triggerCounterUse)

triggerCounterUse :: EntUse
triggerCounterUse = EntUse "trigger_counter_use" $ \selfRef _ activatorRef -> do
    self <- readRef selfRef
    doTriggerCounterUse selfRef self activatorRef
  where
    doTriggerCounterUse selfRef self Nothing =
        Com.fatalError "GameTrigger.triggerCounterUse activatorRef is Nothing"
    doTriggerCounterUse selfRef self (Just activatorRef) =
        unless ((self^.eCount) == 0) $ do
            gameImport <- use (gameBaseGlobals.gbGameImport)
            let count = (self^.eCount) - 1
                sound = gameImport^.giSound
                soundIndex = gameImport^.giSoundIndex
                centerPrintf = gameImport^.giCenterPrintf
            modifyRef selfRef (\v -> v & eCount .~ count)
            if count > 0
                then
                    when ((self^.eSpawnFlags) .&. 1 == 0) $ do
                        centerPrintf activatorRef (encode count `B.append` " more to go...")
                        soundIdx <- soundIndex (Just "misc/talk1.wav")
                        sound (Just activatorRef) Constants.chanAuto soundIdx 1 Constants.attnNorm 0
                else do
                  when ((self^.eSpawnFlags) .&. 1 == 0) $ do
                      centerPrintf activatorRef "Sequence completed!"
                      soundIdx <- soundIndex (Just "misc/talk1.wav")
                      sound (Just activatorRef) Constants.chanAuto soundIdx 1 Constants.attnNorm 0
                  modifyRef selfRef (\v -> v & eActivator .~ (Just activatorRef))
                  multiTrigger selfRef

spTriggerGravity :: Ref EdictT -> Quake ()
spTriggerGravity selfRef = do
    gravity <- use (gameBaseGlobals.gbSpawnTemp.stGravity)
    maybe noGravity hasGravity gravity
  where
    noGravity = do
        self <- readRef selfRef
        dprintf <- use (gameBaseGlobals.gbGameImport.giDprintf)
        dprintf (B.concat ["trigger_gravity without gravity set at ", Lib.vtos (self^.eEntityState.esOrigin), "\n"])
        GameUtil.freeEdict selfRef
    hasGravity gravity = do
        initTrigger selfRef
        modifyRef selfRef (\v -> v & eGravity .~ fromIntegral (Lib.atoi gravity)
                                   & eTouch .~ Just triggerGravityTouch)

triggerGravityTouch :: EntTouch
triggerGravityTouch = EntTouch "trigger_gravity_touch" $ \selfRef otherRef _ _ -> do
    self <- readRef selfRef
    modifyRef otherRef (\v -> v & eGravity .~ (self^.eGravity))

spTriggerHurt :: Ref EdictT -> Quake ()
spTriggerHurt selfRef = do
    initTrigger selfRef
    self <- readRef selfRef
    gameImport <- use (gameBaseGlobals.gbGameImport)
    soundIdx <- (gameImport^.giSoundIndex) (Just "world/electro.wav")
    let solid = if (self^.eSpawnFlags) .&. 1 /= 0 then Constants.solidNot else Constants.solidTrigger
        selfUse = if (self^.eSpawnFlags) .&. 2 /= 0 then Just hurtUse else self^.eUse
    modifyRef selfRef (\v -> v & eNoiseIndex .~ soundIdx
                               & eTouch .~ Just hurtTouch
                               & eDmg %~ (\a -> if a == 0 then 5 else a)
                               & eSolid .~ solid
                               & eUse .~ selfUse)
    (gameImport^.giLinkEntity) selfRef

hurtUse :: EntUse
hurtUse = EntUse "hurt_use" $ \selfRef _ _ -> do
    self <- readRef selfRef
    let solid = if (self^.eSolid) == Constants.solidNot then Constants.solidTrigger else Constants.solidNot
    modifyRef selfRef (\v -> v & eSolid .~ solid)
    linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
    linkEntity selfRef
    updatedSelf <- readRef selfRef
    when ((updatedSelf^.eSpawnFlags) .&. 2 == 0) $
        modifyRef selfRef (\v -> v & eUse .~ Nothing)

hurtTouch :: EntTouch
hurtTouch = EntTouch "hurt_touch" $ \selfRef otherRef _ _ -> do
    self <- readRef selfRef
    other <- readRef otherRef
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    when (shouldProceed self other levelTime) $ do
        let timeStamp = if (self^.eSpawnFlags) .&. 16 /= 0 then levelTime + 1 else levelTime + Constants.frameTime
            dFlags = if (self^.eSpawnFlags) .&. 8 /= 0 then Constants.damageNoProtection else 0
        v3o <- use (globals.gVec3Origin)
        frameNum <- use (gameBaseGlobals.gbLevel.llFrameNum)
        modifyRef selfRef (\v -> v & eTimeStamp .~ timeStamp)
        when ((self^.eSpawnFlags) .&. 4 == 0 && frameNum `mod` 10 == 0) $ do
            sound <- use (gameBaseGlobals.gbGameImport.giSound)
            sound (Just otherRef) Constants.chanAuto (self^.eNoiseIndex) 1 Constants.attnNorm 0
        GameCombat.damage otherRef selfRef selfRef v3o (other^.eEntityState.esOrigin) v3o (self^.eDmg) (self^.eDmg) dFlags Constants.modTriggerHurt
  where
    shouldProceed self other levelTime
        | (other^.eTakeDamage) == 0 = False
        | (self^.eTimeStamp) > levelTime = False
        | otherwise = True

spTriggerKey :: Ref EdictT -> Quake ()
spTriggerKey selfRef = do
    self <- readRef selfRef
    item <- use (gameBaseGlobals.gbSpawnTemp.stItem)
    maybe (noItem self) (hasItem self) item
  where
    noItem self = do
        dprintf <- use (gameBaseGlobals.gbGameImport.giDprintf)
        dprintf (B.concat ["no key item for trigger_key at ", Lib.vtos (self^.eEntityState.esOrigin), "\n"])
    hasItem self item = do
        foundItemRef <- GameItems.findItemByClassname item
        modifyRef selfRef (\v -> v & eItem .~ foundItemRef)
        maybe (itemNotFound self item) (foundItem self) foundItemRef
    itemNotFound self item = do
        dprintf <- use (gameBaseGlobals.gbGameImport.giDprintf)
        dprintf (B.concat ["item ", item, " not found for trigger_key at ", Lib.vtos (self^.eEntityState.esOrigin), "\n"])
    foundItem self _ =
        maybe (noTarget self) hasTarget (self^.eTarget)
    noTarget self = do
        dprintf <- use (gameBaseGlobals.gbGameImport.giDprintf)
        dprintf (B.concat [self^.eClassName, " at ", Lib.vtos (self^.eEntityState.esOrigin), " has no target\n"])
    hasTarget _ = do
        soundIndex <- use (gameBaseGlobals.gbGameImport.giSoundIndex)
        void (soundIndex (Just "misc/keytry.wav"))
        void (soundIndex (Just "misc/keyuse.wav"))
        modifyRef selfRef (\v -> v & eUse .~ Just triggerKeyUse)

triggerKeyUse :: EntUse
triggerKeyUse = error "GameTrigger.triggerKeyUse" -- TODO

spTriggerMonsterJump :: Ref EdictT -> Quake ()
spTriggerMonsterJump selfRef = do
    modifyRef selfRef (\v -> v & eSpeed %~ (\a -> if a == 0 then 200 else a)
                               & eEntityState.esAngles._y %~ (\a -> if a == 0 then 360 else a))
    gameBaseGlobals.gbSpawnTemp.stHeight %= (\v -> if v == 0 then 200 else v)
    height <- use (gameBaseGlobals.gbSpawnTemp.stHeight)
    initTrigger selfRef
    modifyRef selfRef (\v -> v & eTouch .~ Just triggerMonsterJumpTouch
                               & eMoveDir._z .~ fromIntegral height)

triggerMonsterJumpTouch :: EntTouch
triggerMonsterJumpTouch = EntTouch "trigger_monsterjump_touch" $ \selfRef otherRef _ _ -> do
    self <- readRef selfRef
    other <- readRef otherRef
    when (shouldProceed other) $ do
        -- set XY even if not on ground, so the jump will clear lips
        modifyRef otherRef (\v -> v & eVelocity._x .~ (self^.eMoveDir._x) * (self^.eSpeed)
                                    & eVelocity._y .~ (self^.eMoveDir._y) * (self^.eSpeed))
        maybe (modifyRef otherRef (\v -> v & eVelocity._z .~ (self^.eMoveDir._z)))
              (\_ -> return ())
              (other^.eGroundEntity)
  where
    shouldProceed other
        | (other^.eFlags) .&. (Constants.flFly .|. Constants.flSwim) /= 0 = False
        | (other^.eSvFlags) .&. Constants.svfDeadMonster /= 0 = False
        | (other^.eSvFlags) .&. Constants.svfMonster == 0 = False
        | otherwise = True

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
multiTrigger edictRef = do
    edict <- readRef edictRef
    when ((edict^.eNextThink) == 0) $ do
        GameUtil.useTargets edictRef (edict^.eActivator)
        updatedEdict <- readRef edictRef
        levelTime <- use (gameBaseGlobals.gbLevel.llTime)
        if (updatedEdict^.eWait) > 0
            then
                modifyRef edictRef (\v -> v & eThink .~ Just multiWait
                                            & eNextThink .~ levelTime + (updatedEdict^.eWait))
            else
                -- we can't just remove (self) here, because this is a touch
                -- function called while looping through area links...
                modifyRef edictRef (\v -> v & eTouch .~ Nothing
                                            & eNextThink .~ levelTime + (Constants.frameTime)
                                            & eThink .~ Just GameUtil.freeEdictA)

multiWait :: EntThink
multiWait = EntThink "multi_wait" $ \edictRef -> do
    modifyRef edictRef (\v -> v & eNextThink .~ 0)
    return True

touchMulti :: EntTouch
touchMulti = EntTouch "Touch_Multi" $ \edictRef otherRef _ _ -> do
    edict <- readRef edictRef
    other <- readRef otherRef
    unless (checkSpawnFlags edict other) $ do
        v3o <- use (globals.gVec3Origin)
        unless (checkMoveDir edict other v3o) $ do
            modifyRef edictRef (\v -> v & eActivator .~ Just otherRef)
            multiTrigger edictRef
  where
    checkSpawnFlags edict other
        | isJust (other^.eClient) = (edict^.eSpawnFlags) .&. 2 /= 0
        | (other^.eSvFlags) .&. Constants.svfMonster /= 0 = (edict^.eSpawnFlags) .&. 1 == 0
        | otherwise = True
    checkMoveDir edict other v3o
        | (edict^.eMoveDir) /= v3o =
            let (forward, _, _) = Math3D.angleVectors (other^.eEntityState.esAngles) True False False
            in dot forward (edict^.eMoveDir) < 0
        | otherwise = False

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
spTriggerPush selfRef = do
    initTrigger selfRef
    gameImport <- use $ gameBaseGlobals.gbGameImport
    windSound <- (gameImport^.giSoundIndex) (Just "misc/windfly.wav")
    gameBaseGlobals.gbWindSound .= windSound
    modifyRef selfRef (\v -> v & eTouch .~ Just triggerPushTouch
                               & eSpeed %~ (\a -> if a == 0 then 1000 else a))
    (gameImport^.giLinkEntity) selfRef

triggerPushTouch :: EntTouch
triggerPushTouch = EntTouch "trigger_push_touch" $ \selfRef otherRef _ _ -> do
    self <- readRef selfRef
    other <- readRef otherRef
    doTriggerPushTouch selfRef self otherRef other
    when ((self^.eSpawnFlags) .&. pushOnce /= 0) $
        GameUtil.freeEdict selfRef
  where
    doTriggerPushTouch selfRef self otherRef other
        | (other^.eClassName) == "grenade" =
            modifyRef otherRef (\v -> v & eVelocity .~ fmap (* (10 * (self^.eSpeed))) (self^.eMoveDir))
        | (other^.eHealth) > 0 = do
            modifyRef otherRef (\v -> v & eVelocity .~ fmap (* (10 * (self^.eSpeed))) (self^.eMoveDir))
            maybe (return ()) (noImmediateFallingDamage otherRef other) (other^.eClient)
        | otherwise = return ()
    noImmediateFallingDamage otherRef other gClientRef = do
        modifyRef gClientRef (\v -> v & gcOldVelocity .~ (other^.eVelocity))
        levelTime <- use (gameBaseGlobals.gbLevel.llTime)
        when ((other^.eFlySoundDebounceTime) < levelTime) $ do
            modifyRef otherRef (\v -> v & eFlySoundDebounceTime .~ levelTime + 1.5)
            windSound <- use (gameBaseGlobals.gbWindSound)
            sound <- use (gameBaseGlobals.gbGameImport.giSound)
            sound (Just otherRef) Constants.chanAuto windSound 1 Constants.attnNorm 0

spTriggerRelay :: Ref EdictT -> Quake ()
spTriggerRelay edictRef =
    modifyRef edictRef (\v -> v & eUse .~ Just triggerRelayUse)

triggerRelayUse :: EntUse
triggerRelayUse = EntUse "trigger_relay_use" $ \selfRef _ activatorRef -> do
    GameUtil.useTargets selfRef activatorRef

initTrigger :: Ref EdictT -> Quake ()
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
