module Game.GameUtil
    ( clearEdict
    , findTarget
    , freeEdict
    , freeEdictA
    , inFront
    , killBox
    , mCheckAttack
    , megaHealthThink
    , monsterUse
    , range
    , spawn
    , useTargets
    , validateSelectedItem
    , visible
    ) where

import           Control.Lens          (use, (^.), (+=), (&), (.~), (+~), (%~))
import           Control.Monad         (when, unless, void)
import           Data.Bits             (complement, (.&.), (.|.))
import qualified Data.ByteString.Char8 as BC
import           Data.Char             (toLower)
import           Data.Maybe            (isNothing, isJust, fromJust)
import           Linear                (dot, normalize, norm, _z)

import qualified Client.M              as M
import qualified Constants
import           Game.CVarT
import           Game.EdictT
import           Game.EntityStateT
import           Game.GameLocalsT
import           Game.LevelLocalsT
import           Game.MonsterInfoT
import           Game.TraceT
import qualified QCommon.Com           as Com
import           QCommon.CVarVariables
import           QuakeRef
import           QuakeState
import           Types
import qualified Util.Math3D           as Math3D

import {-# SOURCE #-} qualified Game.GameBase as GameBase

clearEdict :: Ref EdictT -> Quake ()
clearEdict edictRef = do
    edict <- readRef edictRef
    writeRef edictRef (newEdictT (edict^.eIndex))

freeEdict :: Ref EdictT -> Quake ()
freeEdict edictRef = do
    edict <- readRef edictRef
    unlinkEntity <- use (gameBaseGlobals.gbGameImport.giUnlinkEntity)
    unlinkEntity edictRef
    maxClients <- fmap (truncate . (^.cvValue)) maxClientsCVar
    when ((edict^.eIndex) > maxClients + Constants.bodyQueueSize) $ do
        levelTime <- use (gameBaseGlobals.gbLevel.llTime)
        writeRef edictRef ((newEdictT (edict^.eIndex)) & eClassName .~ "freed"
                                                       & eFreeTime .~ levelTime
                                                       & eInUse .~ False)

freeEdictA :: EntThink
freeEdictA = EntThink "G_FreeEdictA" $ \edictRef -> do
    freeEdict edictRef
    return False

validateSelectedItem :: Ref EdictT -> Quake ()
validateSelectedItem = error "GameUtil.validateSelectedItem" -- TODO

megaHealthThink :: EntThink
megaHealthThink = error "GameUtil.megaHealthThink" -- TODO

spawn :: Quake (Ref EdictT)
spawn = do
    maxClients <- fmap (truncate . (^.cvValue)) maxClientsCVar
    numEdicts <- use (gameBaseGlobals.gbNumEdicts)
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    edictRef <- findFreeEdict levelTime (maxClients + 1) numEdicts
    maybe (notFoundRef numEdicts) initRef edictRef
  where
    notFoundRef numEdicts = do
        maxEntities <- use (gameBaseGlobals.gbGame.glMaxEntities)
        when (numEdicts == maxEntities) $ do
            err <- use (gameBaseGlobals.gbGameImport.giError)
            err "ED_Alloc: no free edicts"
        initRef (Ref numEdicts)
    initRef edictRef@(Ref idx) = do
        writeRef edictRef (newEdictT idx)
        gameBaseGlobals.gbNumEdicts += 1
        initEdict edictRef
        return edictRef

findFreeEdict :: Float -> Int -> Int -> Quake (Maybe (Ref EdictT))
findFreeEdict levelTime idx maxIdx
    | idx >= maxIdx = return Nothing
    | otherwise = checkFree =<< readRef (Ref idx)
  where
    checkFree edict
        | not (edict^.eInUse) && ((edict^.eFreeTime) < 2 || levelTime - (edict^.eFreeTime) > 0.5) = return (Just (Ref idx))
        | otherwise = findFreeEdict levelTime (idx + 1) maxIdx

initEdict :: Ref EdictT -> Quake ()
initEdict edictRef@(Ref idx) =
    modifyRef edictRef (\v -> v & eInUse .~ True
                                & eClassName .~ "noclass"
                                & eGravity .~ 1.0
                                & eEntityState .~ (newEntityStateT (Just edictRef) & esNumber .~ idx))

inFront :: EdictT -> EdictT -> Bool
inFront self other =
    let (forward, _, _) = Math3D.angleVectors (self^.eEntityState.esAngles) True False False
        vec = normalize ((other^.eEntityState.esOrigin) - (self^.eEntityState.esOrigin))
        dot' = vec `dot` forward
    in dot' > 0.3

useTargets :: Ref EdictT -> Maybe (Ref EdictT) -> Quake ()
useTargets edictRef activatorRef = do
    edict <- readRef edictRef
    doUseTargets edict
  where
    doUseTargets edict
        | (edict^.eDelay) /= 0 = do
            -- create a temp object to fire at a later time
            tmpRef <- spawn
            levelTime <- use (gameBaseGlobals.gbLevel.llTime)
            when (isNothing activatorRef) $ do
                dprintf <- use (gameBaseGlobals.gbGameImport.giDprintf)
                dprintf "Think_Delay with no activator\n"
            modifyRef tmpRef (\v -> v & eClassName .~ "DelayedUse"
                                      & eNextThink .~ levelTime + (edict^.eDelay)
                                      & eThink .~ Just thinkDelay
                                      & eActivator .~ activatorRef
                                      & eMessage .~ (edict^.eMessage)
                                      & eTarget .~ (edict^.eTarget)
                                      & eKillTarget .~ (edict^.eKillTarget))
        | otherwise =
            maybe activatorError (proceedUseTargets edict) activatorRef
    activatorError = Com.fatalError "GameUtil.useTargets activatorRef is Nothing"
    proceedUseTargets edict activatorRef = do
        activator <- readRef activatorRef
        printMessage edict activatorRef activator (edict^.eMessage)
        done <- maybe (return False) (killKillTargets Nothing) (edict^.eKillTarget)
        unless done $ do
            maybe (return ()) (fireTargets (BC.map toLower (edict^.eClassName)) Nothing GameBase.findByTarget) (edict^.eTarget)
    printMessage _ _ _ Nothing = return ()
    printMessage edict activatorRef activator (Just message)
        | ((activator^.eSvFlags) .&. Constants.svfMonster) == 0 = do
            centerPrintf <- use (gameBaseGlobals.gbGameImport.giCenterPrintf)
            centerPrintf activatorRef message
            playSound activatorRef edict
        | otherwise = return ()
    playSound activatorRef edict
        | (edict^.eNoiseIndex) /= 0 = do
            sound <- use (gameBaseGlobals.gbGameImport.giSound)
            sound (Just activatorRef) Constants.chanAuto (edict^.eNoiseIndex) 1 Constants.attnNorm 0
        | otherwise = do
            gameImport <- use (gameBaseGlobals.gbGameImport)
            talkIdx <- (gameImport^.giSoundIndex) (Just "misc/talk1.wav")
            (gameImport^.giSound) (Just activatorRef) Constants.chanAuto talkIdx 1 Constants.attnNorm 0
    killKillTargets entRef killTarget = do
        nextRef <- GameBase.gFind entRef GameBase.findByTarget killTarget
        maybe (return False) (checkTarget killTarget) nextRef
    checkTarget killTarget newRef = do
        freeEdict newRef
        edict <- readRef edictRef
        proceedKillTargets newRef edict killTarget
    proceedKillTargets newRef edict killTarget
        | edict^.eInUse = killKillTargets (Just newRef) killTarget
        | otherwise = do
            dprintf <- use (gameBaseGlobals.gbGameImport.giDprintf)
            dprintf "entity was removed while using killtargets\n"
            return True
    -- TODO: refactor this, just an old copy, too tired to do it now
    fireTargets edictClassName ref findBy targetName = do
        foundRef <- GameBase.gFind ref findBy targetName
        when (isJust foundRef) $ do
            let Just foundEdictRef = foundRef
            foundEdict <- readRef foundEdictRef
            -- doors fire area portals in a specific way
            let foundEdictClassName = BC.map toLower (foundEdict^.eClassName)
            if foundEdictClassName == "func_areaportal" && (any (== edictClassName) ["func_door", "func_door_rotating"])
                then
                    fireTargets edictClassName foundRef findBy targetName
                else do
                    dprintf <- use (gameBaseGlobals.gbGameImport.giDprintf)
                    if foundEdictRef == edictRef
                        then
                            dprintf "WARNING: Entity used iteself.\n"
                        else
                            when (isJust (foundEdict^.eUse)) $
                                entUse (fromJust $ foundEdict^.eUse) foundEdictRef (Just edictRef) activatorRef
                    edict <- readRef edictRef
                    if not (edict^.eInUse)
                        then dprintf "entity was removed while using targets\n"
                        else fireTargets edictClassName foundRef findBy targetName

killBox :: Ref EdictT -> Quake Bool
killBox = error "GameUtil.killBox" -- TODO

monsterUse :: EntUse
monsterUse = EntUse "monster_use" $ \selfRef _ (Just activatorRef) -> do
    self <- readRef selfRef
    activator <- readRef activatorRef
    let done = isJust (self^.eEnemy) || (self^.eHealth) <= 0 || ((activator^.eFlags) .&. Constants.flNoTarget) /= 0 || (isNothing (activator^.eClient) && ((activator^.eMonsterInfo.miAIFlags) .&. Constants.aiGoodGuy) == 0)
    unless done $ do
        modifyRef selfRef (\v -> v & eEnemy .~ Just activatorRef)
        foundTarget selfRef

foundTarget :: Ref EdictT -> Quake ()
foundTarget = error "GameUtil.foundTarget" -- TODO

mCheckAttack :: EntThink
mCheckAttack = error "GameUtil.mCheckAttack" -- TODO

{-
- Returns the range catagorization of an entity reletive to self 0 melee
- range, will become hostile even if back is turned 1 visibility and
- infront, or visibility and show hostile 2 infront and show hostile 3 only
- triggered by damage.
-}
range :: EdictT -> EdictT -> Int
range self other
    | len < (fromIntegral Constants.meleeDistance) = Constants.rangeMelee
    | len < 500 = Constants.rangeNear
    | len < 1000 = Constants.rangeMid
    | otherwise = Constants.rangeFar
  where
    v = (self^.eEntityState.esOrigin) - (other^.eEntityState.esOrigin)
    len = norm v

visible :: Ref EdictT -> Ref EdictT -> Quake Bool
visible selfRef otherRef = do
    self <- readRef selfRef
    other <- readRef otherRef
    let spot1 = (self^.eEntityState.esOrigin) & _z +~ fromIntegral (self^.eViewHeight)
        spot2 = (other^.eEntityState.esOrigin) & _z +~ fromIntegral (other^.eViewHeight)
    v3o <- use (globals.gVec3Origin)
    trace <- use (gameBaseGlobals.gbGameImport.giTrace)
    traceT <- trace spot1 (Just v3o) (Just v3o) spot2 (Just selfRef) Constants.maskOpaque
    return ((traceT^.tFraction) == 1)

thinkDelay :: EntThink
thinkDelay = EntThink "Think_Delay" $ \edictRef -> do
    edict <- readRef edictRef
    useTargets edictRef (edict^.eActivator)
    freeEdict edictRef
    return True

findTarget :: Ref EdictT -> Quake Bool
findTarget selfRef = do
    self <- readRef selfRef
    doFindTarget self
  where
    doFindTarget self
        | (self^.eMonsterInfo.miAIFlags) .&. Constants.aiGoodGuy /= 0 = return False
        | otherwise = checkCombatPoint self
    checkCombatPoint self
        | (self^.eMonsterInfo.miAIFlags) .&. Constants.aiCombatPoint /= 0 = return False
        | otherwise = checkHearNoise self =<< use (gameBaseGlobals.gbLevel)
    checkHearNoise self level
        | (level^.llSightEntityFrameNum) >= ((level^.llFrameNum) - 1) && (self^.eSpawnFlags) .&. 1 == 0 =
            maybe sightEntityError (doCheckHearNoise self) (level^.llSightEntity)
        | (level^.llSoundEntityFrameNum) >= ((level^.llFrameNum) - 1) =
            maybe soundEntityError (\se -> checkClientInUse se True) (level^.llSoundEntity)
        | isJust (self^.eEnemy) && (level^.llSound2EntityFrameNum) >= ((level^.llFrameNum) - 1) && (self^.eSpawnFlags) .&. 1 /= 0 =
            maybe sound2EntityError (\s2e -> checkClientInUse s2e True) (level^.llSound2Entity)
        | otherwise = maybe (return False) (\clientRef -> checkClientInUse clientRef False) (level^.llSightClient)
    sightEntityError = do
        Com.fatalError "GameUtil.findTarget#checkHearNoise level^.llSightEntity is Nothing"
        return False
    soundEntityError = do
        Com.fatalError "GameUtil.findTarget#checkHearNoise level^.llSoundEntity is Nothing"
        return False
    sound2EntityError = do
        Com.fatalError "GameUtil.findTarget#checkHearNoise level^.llSound2Entity is Nothing"
        return False
    doCheckHearNoise self clientRef = do
        client <- readRef clientRef
        if (client^.eEnemy) == (self^.eEnemy)
            then return False
            else checkClientInUse clientRef False
    checkClientInUse clientRef heardIt = do
        client <- readRef clientRef
        if not (client^.eInUse)
            then return False
            else checkClientFlags clientRef client heardIt
    checkClientFlags clientRef client heardIt
        | isJust (client^.eClient) && (client^.eFlags) .&. Constants.flNoTarget /= 0 = return False
        | isJust (client^.eClient) = actBasedOnHeardIt clientRef heardIt
        | (client^.eSvFlags) .&. Constants.svfMonster /= 0 = maybe (return False) (doCheckClientFlags clientRef heardIt) (client^.eEnemy)
        | heardIt = maybe ownerError (doCheckClientFlags clientRef heardIt) (client^.eOwner)
        | otherwise = return False
    ownerError = do
        Com.fatalError "GameUtil.findTarget#checkClientFlags client^.eOwner is Nothing"
        return False
    doCheckClientFlags clientRef heardIt entRef = do
        ent <- readRef entRef
        if (ent^.eFlags) .&. Constants.flNoTarget /= 0
            then return False
            else actBasedOnHeardIt clientRef heardIt
    actBasedOnHeardIt clientRef heardIt = do
        self <- readRef selfRef
        client <- readRef clientRef
        vis <- visible selfRef clientRef
        levelTime <- use (gameBaseGlobals.gbLevel.llTime)
        if not heardIt
            then didntHear clientRef client self (range self client) vis levelTime
            else didHear clientRef client self vis
    didntHear clientRef client self r vis levelTime
        | r == Constants.rangeFar || (client^.eLightLevel) <= 5 || not vis = return False
        | r == Constants.rangeNear && fromIntegral (client^.eShowHostile) < levelTime && not (inFront self client) = return False
        | r == Constants.rangeMid && not (inFront self client) = return False
        | Just clientRef == (self^.eEnemy) = return True -- JDC false
        | otherwise = do
            modifyRef selfRef (\v -> v & eEnemy .~ Just clientRef)
            if (client^.eClassName) /= "player_noise"
                then do
                  modifyRef selfRef (\v -> v & eMonsterInfo.miAIFlags %~ (.&. (complement Constants.aiSoundTarget)))
                  maybe (proceedDidntHear client) (\_ -> finishFindTarget) (client^.eClient)
                else
                  finishFindTarget
    proceedDidntHear client = do
        modifyRef selfRef (\v -> v & eEnemy .~ (client^.eEnemy))
        maybe enemyError finishDidntHear (client^.eEnemy)
    enemyError = do
        Com.fatalError "GameUtil.findTarget#proceedDidntHear client^.eEnemy is Nothing"
        return False
    finishDidntHear enemyRef = do
        enemy <- readRef enemyRef
        maybe unsetEnemy (\_ -> finishFindTarget) (enemy^.eClient)
    unsetEnemy = do
        modifyRef selfRef (\v -> v & eEnemy .~ Nothing)
        return False
    didHear clientRef client self vis
        | (self^.eSpawnFlags) .&. 1 /= 0 && not vis = return False
        | otherwise = do
            inPHS <- use (gameBaseGlobals.gbGameImport.giInPHS)
            v <- inPHS (self^.eEntityState.esOrigin) (client^.eEntityState.esOrigin)
            proceedDidHear clientRef client self v ((client^.eEntityState.esOrigin) - (self^.eEntityState.esOrigin))
    proceedDidHear clientRef client self v temp
        | not v = return False
        | norm temp > 1000 = return False
        | otherwise = do
            done <- checkAreasConnected self client
            finishDidHear clientRef self done temp
    finishDidHear clientRef self done temp
        | done = return False
        | otherwise = do
            modifyRef selfRef (\vv -> vv & eIdealYaw .~ Math3D.vectorYaw temp)
            M.changeYaw selfRef
            modifyRef selfRef (\vv -> vv & eMonsterInfo.miAIFlags %~ (.|. Constants.aiSoundTarget))
            if Just clientRef == (self^.eEnemy)
              then
                return True
              else do
                modifyRef selfRef (\vv -> vv & eEnemy .~ Just clientRef)
                finishFindTarget
    checkAreasConnected self client
        | (client^.eAreaNum) /= (self^.eAreaNum) = do
            areasConnected <- use (gameBaseGlobals.gbGameImport.giAreasConnected)
            connected <- areasConnected (self^.eAreaNum) (client^.eAreaNum)
            return (not connected)
        | otherwise =
            return False
    finishFindTarget = do
      foundTarget selfRef
      self <- readRef selfRef
      when ((self^.eMonsterInfo.miAIFlags) .&. Constants.aiSoundTarget == 0 && isJust (self^.eMonsterInfo.miSight)) $
          targetInteraction (self^.eMonsterInfo.miSight) (self^.eEnemy)
      return True
    targetInteraction Nothing _ = Com.fatalError "GameUtil.findTarget#finishFindTarget self^.eMonsterInfo.miSight is Nothing"
    targetInteraction _ Nothing = Com.fatalError "GameUtil.findTarget#finishFindTarget self^.eEnemy is Nothing"
    targetInteraction (Just sightF) (Just enemyRef) = -- IMPROVE: are we sure eEnemy is Just ?
          void (entInteract sightF selfRef enemyRef)
