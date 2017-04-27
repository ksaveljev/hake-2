module Server.SV
    ( moveStep
    , physicsNoClip
    , physicsNone
    , physicsPusher
    , physicsStep
    , physicsToss
    ) where

import           Control.Lens          (use, (^.), (.=), (+=), (-=), (&), (.~), (+~), (-~), (%~))
import           Control.Monad         (when, void, unless)
import           Data.Bits             (complement, (.&.), (.|.))
import           Data.Maybe            (isJust, isNothing)
import qualified Data.Vector           as V
import           Linear                (V3(..), cross, dot, _x, _y, _z)

import qualified Constants
import           Game.CPlaneT
import           Game.CVarT
import           Game.EdictT
import           Game.EntityStateT
import           Game.GClientT
import           Game.LevelLocalsT
import           Game.LinkT
import           Game.MonsterInfoT
import           Game.PlayerStateT
import           Game.PMoveStateT
import           Game.PushedT
import           Game.TraceT
import qualified QCommon.Com           as Com
import           QCommon.CVarVariables
import           QuakeRef
import           QuakeState
import           Types
import qualified Util.Math3D           as Math3D

import {-# SOURCE #-} qualified Client.M      as M
import {-# SOURCE #-} qualified Game.GameBase as GameBase

physicsPusher :: Ref EdictT -> Quake ()
physicsPusher edictRef = do
    edict <- readRef edictRef
    -- if not a team captain, so movement will be handled elsewhere
    unless ((edict^.eFlags) .&. Constants.flTeamSlave /= 0) $ do
        -- make sure all team slaves can move before commiting
        -- any moves or calling any think functions
        -- if the move is blocked, all moved objects will be backed out
        -- retry:
        gameBaseGlobals.gbPushedP .= 0
        part <- pushTeamChain (Just edictRef)
        checkMemoryCorruption
        maybe (moveSucceeded edictRef) (moveFailed edictRef) part

pushTeamChain :: Maybe (Ref EdictT) -> Quake (Maybe (Ref EdictT))
pushTeamChain Nothing = return Nothing
pushTeamChain (Just edictRef) = proceedPushTeamChain edictRef =<< readRef edictRef

proceedPushTeamChain :: Ref EdictT -> EdictT -> Quake (Maybe (Ref EdictT))
proceedPushTeamChain edictRef edict
    | isMoving = checkPushed =<< push edictRef move amove
    | otherwise = pushTeamChain (edict^.eTeamChain)
  where
    velocity = edict^.eVelocity
    avelocity = edict^.eAVelocity
    move = fmap (* Constants.frameTime) velocity
    amove = fmap (* Constants.frameTime) avelocity
    isMoving = (velocity^._x) /= 0 || (velocity^._y) /= 0 || (velocity^._z) /= 0 || (avelocity^._x) /= 0 || (avelocity^._y) /= 0 || (avelocity^._z) /= 0
    checkPushed True = pushTeamChain (edict^.eTeamChain)
    checkPushed False = return (Just edictRef) -- move was blocked

checkMemoryCorruption :: Quake ()
checkMemoryCorruption = do
    pushedP <- use (gameBaseGlobals.gbPushedP)
    when (pushedP > Constants.maxEdicts) $ do
        err <- use (gameBaseGlobals.gbGameImport.giError2)
        err Constants.errFatal "pushed_p > &pushed[MAX_EDICTS], memory corrupted"

moveSucceeded :: Ref EdictT -> Quake ()
moveSucceeded edictRef = thinkTeamChain (Just edictRef)

-- the move failed, bump all nextthink times and back out moves
moveFailed :: Ref EdictT -> Ref EdictT -> Quake ()
moveFailed edictRef blockedRef = do
    backOutTeamChain (Just edictRef)
    blockedEdict <- readRef blockedRef
    obstacle <- use (gameBaseGlobals.gbObstacle)
    maybe (return ()) (runBlocked obstacle) (blockedEdict^.eBlocked)
  where
    runBlocked Nothing _ = Com.fatalError "SV.moveFailed obstacle is Nothing"
    runBlocked (Just obstacle) edictBlockedF = entBlocked edictBlockedF blockedRef obstacle

thinkTeamChain :: Maybe (Ref EdictT) -> Quake ()
thinkTeamChain Nothing = return ()
thinkTeamChain (Just edictRef) = do
    edict <- readRef edictRef
    void (runThink edictRef)
    thinkTeamChain (edict^.eTeamChain)

backOutTeamChain :: Maybe (Ref EdictT) -> Quake ()
backOutTeamChain Nothing = return ()
backOutTeamChain (Just edictRef) = do
    edict <- readRef edictRef
    when ((edict^.eNextThink) > 0) $
        modifyRef edictRef (\v -> v & eNextThink +~ Constants.frameTime)
    backOutTeamChain (edict^.eTeamChain)

physicsNone :: Ref EdictT -> Quake ()
physicsNone = void . runThink -- regular thinking

-- A moving object that doesn't obey physics.
physicsNoClip :: Ref EdictT -> Quake ()
physicsNoClip edictRef = do
    ok <- runThink edictRef
    when ok $ do
        edict <- readRef edictRef
        modifyRef edictRef (\v -> v & eEntityState.esAngles .~ (edict^.eEntityState.esAngles) + fmap (* Constants.frameTime) (edict^.eAVelocity)
                                    & eEntityState.esOrigin .~ (edict^.eEntityState.esOrigin) + fmap (* Constants.frameTime) (edict^.eVelocity))
        linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
        linkEntity edictRef

physicsStep :: Ref EdictT -> Quake ()
physicsStep edictRef = do
    -- airborn monsters should always check for ground
    wasOnGround <- checkGroundEntity =<< readRef edictRef
    checkVelocity edictRef =<< readRef edictRef
    checkFriction =<< readRef edictRef
    -- add gravity except:
    --   flying monsters
    --   swiming monsters who are in the water
    hitSound <- checkGravity wasOnGround =<< readRef edictRef
    -- friction for flying monsters that have been given vertical velocity
    checkFlyingFriction =<< readRef edictRef
    -- friction for flying monsters that have been given vertical velocity
    checkSwimmingFriction =<< readRef edictRef
    edict <- readRef edictRef
    doPhysicsStep edict (edict^.eVelocity) wasOnGround hitSound
  where
    checkGroundEntity edict = do
        when (isNothing (edict^.eGroundEntity)) $
            M.checkGround edictRef
        edict' <- readRef edictRef
        maybe (return False) (\_ -> return True) (edict'^.eGroundEntity)
    checkFriction edict = do
        let V3 a b c = edict^.eAVelocity
        when (a /= 0 || b /= 0 || c /= 0) $
            addRotationalFriction edictRef
    checkGravity wasOnGround edict
        | not wasOnGround && (edict^.eFlags) .&. Constants.flFly == 0 && not ((edict^.eFlags) .&. Constants.flSwim /= 0 && (edict^.eWaterLevel) > 2) = do
            svGravity <- fmap (^.cvValue) svGravityCVar
            when ((edict^.eWaterLevel) == 0) $
                addGravity edictRef
            return ((edict^.eVelocity._z) < svGravity * (-0.1))
        | otherwise =
            return False
    checkFlyingFriction edict = do
        when ((edict^.eFlags) .&. Constants.flFly /= 0 && (edict^.eVelocity._z) /= 0) $ do
            let speed = abs (edict^.eVelocity._z)
                control | speed < Constants.svStopSpeed = Constants.svStopSpeed
                        | otherwise = speed
                friction = Constants.svFriction / 3
                newSpeed = speed - (Constants.frameTime * control * friction)
                newSpeed' = if newSpeed < 0 then 0 else newSpeed / speed
            modifyRef edictRef (\v -> v & eVelocity._z %~ (* newSpeed'))
    checkSwimmingFriction edict = do
        when ((edict^.eFlags) .&. Constants.flSwim /= 0 && (edict^.eVelocity._z) /= 0) $ do
            let speed = abs (edict^.eVelocity._z)
                control | speed < Constants.svStopSpeed = Constants.svStopSpeed
                        | otherwise = speed
                newSpeed = speed - (Constants.frameTime * control * Constants.svWaterFriction * (fromIntegral $ edict^.eWaterLevel))
                newSpeed' = if newSpeed < 0 then 0 else newSpeed / speed
            modifyRef edictRef (\v -> v & eVelocity._z %~ (* newSpeed'))
    doPhysicsStep edict (V3 a b c) wasOnGround hitSound
        | a /= 0 || b /= 0 || c /= 0 = do
            -- apply friction
            -- let dead monsters who aren't completely onground slide
            when (wasOnGround || (edict^.eFlags) .&. (Constants.flSwim .|. Constants.flFly) /= 0) $ do
                ok <- M.checkBottom edictRef
                when (not ((edict^.eHealth) <= 0 && not ok)) $ do
                    let vel = edict^.eVelocity
                        speed = sqrt $ (vel^._x) * (vel^._x) + (vel^._y) * (vel^._y)
                    when (speed /= 0) $ do
                        let friction = Constants.svFriction
                            control | speed < Constants.svStopSpeed = Constants.svStopSpeed
                                    | otherwise = speed
                            newSpeed = speed - Constants.frameTime * control * friction
                            newSpeed' = if newSpeed < 0 then 0 else newSpeed / speed
                        modifyRef edictRef (\v -> v & eVelocity._x %~ (* newSpeed')
                                                    & eVelocity._y %~ (* newSpeed'))
            let mask | (edict^.eSvFlags) .&. Constants.svfMonster /= 0 = Constants.maskMonsterSolid
                     | otherwise                                       = Constants.maskSolid
            void (flyMove edictRef Constants.frameTime mask)
            gameImport <- use (gameBaseGlobals.gbGameImport)
            (gameImport^.giLinkEntity) edictRef
            GameBase.touchTriggers edictRef
            updatedEdict <- readRef edictRef
            when (updatedEdict^.eInUse) $ do
                when (isJust (updatedEdict^.eGroundEntity) && not wasOnGround && hitSound) $ do
                    wavIdx <- (gameImport^.giSoundIndex) (Just "world/land.wav")
                    (gameImport^.giSound) (Just edictRef) 0 wavIdx 1 1 0
                void (runThink edictRef)
        | otherwise =
            void (runThink edictRef)

checkVelocity :: Ref EdictT -> EdictT -> Quake ()
checkVelocity edictRef edict = do
    -- bound velocity
    maxVelocity <- fmap (^.cvValue) svMaxVelocityCVar
    modifyRef edictRef (\v -> v & eVelocity .~ (fmap (boundVelocity maxVelocity) (edict^.eVelocity)))
  where 
    boundVelocity maxV v
        | v > maxV    = maxV
        | v < (-maxV) = -maxV
        | otherwise   = v

physicsToss :: Ref EdictT -> Quake ()
physicsToss edictRef = do
    void (runThink edictRef)
    flags <- fmap (^.eFlags) (readRef edictRef)
    unless (flags .&. Constants.flTeamSlave /= 0) $ do
        onGround <- checkGroundEntity
        unless onGround $ do
            oldOrigin <- fmap (^.eEntityState.esOrigin) (readRef edictRef)
            checkVelocity edictRef =<< readRef edictRef
            moveType <- fmap (^.eMoveType) (readRef edictRef)
            addGravityBasedOnMoveType moveType
            moveAngles
            move <- moveOrigin
            traceT <- pushEntity edictRef move
            inUse <- fmap (^.eInUse) (readRef edictRef)
            when inUse $ do
                when ((traceT^.tFraction) < 1) $ do
                    velocity <- fmap (^.eVelocity) (readRef edictRef)
                    let backoff = if moveType == Constants.moveTypeBounce then 1.5 else 1
                        (_, out) = GameBase.clipVelocity velocity (traceT^.tPlane.cpNormal) backoff
                    modifyRef edictRef (\v -> v & eVelocity .~ out)
                    stopIfOnGround moveType traceT
                (wasInWater, isInWater) <- checkWaterTransition
                modifyRef edictRef (\v -> v & eWaterLevel .~ (if isInWater then 1 else 0))
                playWaterSound oldOrigin wasInWater isInWater
                edict <- readRef edictRef
                moveTeamSlaves (edict^.eEntityState.esOrigin) (edict^.eTeamChain)
  where
    checkGroundEntity = do
        velocity <- fmap (^.eVelocity) (readRef edictRef)
        when ((velocity^._z) > 0) $
            modifyRef edictRef (\v -> v & eGroundEntity .~ Nothing)
        edict <- readRef edictRef
        maybe (return False) (\groundEntityRef -> doCheckGroundEntity =<< readRef groundEntityRef) (edict^.eGroundEntity)
    doCheckGroundEntity groundEntity
        | not (groundEntity^.eInUse) = do
            modifyRef edictRef (\v -> v & eGroundEntity .~ Nothing)
            return False
        | otherwise = return True
    addGravityBasedOnMoveType moveType =
        when (moveType /= Constants.moveTypeFly && moveType /= Constants.moveTypeFlyMissile) $
            addGravity edictRef
    moveAngles = do
        edict <- readRef edictRef
        modifyRef edictRef (\v -> v & eEntityState.esAngles .~ ((edict^.eEntityState.esAngles) + fmap (* Constants.frameTime) (edict^.eAVelocity)))
    moveOrigin = do
        edict <- readRef edictRef
        return (fmap (* Constants.frameTime) (edict^.eVelocity))
    stopIfOnGround moveType traceT =
        when ((traceT^.tPlane.cpNormal._z) > 0.7) $ do
            velocity <- fmap (^.eVelocity) (readRef edictRef)
            when ((velocity^._z) < 60 || moveType /= Constants.moveTypeBounce) $ do
              linkCount <- getLinkCount (traceT^.tEnt)
              origin <- use (globals.gVec3Origin)
              modifyRef edictRef (\v -> v & eGroundEntity .~ (traceT^.tEnt)
                                          & eGroundEntityLinkCount .~ linkCount
                                          & eVelocity .~ origin
                                          & eAVelocity .~ origin)
    getLinkCount Nothing = do
        Com.fatalError "SV.physicsToss#stopIfOnGround traceT^.tEnt is Nothing"
        return 0
    getLinkCount (Just traceRef) = fmap (^.eLinkCount) (readRef traceRef)
    checkWaterTransition = do
        edict <- readRef edictRef
        pointContents <- use (gameBaseGlobals.gbGameImport.giPointContents)
        newWaterType <- pointContents (edict^.eEntityState.esOrigin)
        let wasInWater = (edict^.eWaterType) .&. Constants.maskWater /= 0
            isInWater  = newWaterType .&. Constants.maskWater /= 0
        return (wasInWater, isInWater)
    playWaterSound oldOrigin wasInWater isInWater = do
        edict <- readRef edictRef
        soundIndex <- use (gameBaseGlobals.gbGameImport.giSoundIndex)
        hitWav <- soundIndex (Just "misc/h2ohit1.wav")
        doPlayWaterSound edict hitWav oldOrigin wasInWater isInWater
    doPlayWaterSound edict hitWav oldOrigin wasInWater isInWater
        | not wasInWater && isInWater = do
            positionedSound <- use (gameBaseGlobals.gbGameImport.giPositionedSound)
            positionedSound (Just oldOrigin) edictRef Constants.chanAuto hitWav 1 1 0
        | wasInWater && not isInWater = do
            positionedSound <- use (gameBaseGlobals.gbGameImport.giPositionedSound)
            positionedSound (Just (edict^.eEntityState.esOrigin)) edictRef Constants.chanAuto hitWav 1 1 0
        | otherwise = return ()
    moveTeamSlaves _ Nothing = return ()
    moveTeamSlaves origin (Just slaveRef) = do
        modifyRef slaveRef (\v -> v & eEntityState.esOrigin .~ origin)
        linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
        linkEntity slaveRef
        teamChain <- fmap (^.eTeamChain) (readRef slaveRef)
        moveTeamSlaves origin teamChain

-- Runs thinking code for this frame if necessary.
runThink :: Ref EdictT -> Quake Bool
runThink edictRef = do
    edict <- readRef edictRef
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    proceedRunThink edictRef edict levelTime

proceedRunThink :: Ref EdictT -> EdictT -> Float -> Quake Bool
proceedRunThink edictRef edict levelTime
    | (edict^.eNextThink) <= 0 || (edict^.eNextThink) > levelTime + 0.001 = return True
    | otherwise = do
        modifyRef edictRef (\v -> v & eNextThink .~ 0)
        maybe thinkError doThink (edict^.eThink)
        return False
  where
    thinkError = Com.fatalError "SV.proceedRunThink edict^.eThink is Nothing"
    doThink f = void (entThink f edictRef)

push :: Ref EdictT -> V3 Float -> V3 Float -> Quake Bool
push pusherRef move amove = do
    v3o <- use (globals.gVec3Origin)
    let updatedMove = fmap clampMove move
        (forward, right, up) = Math3D.angleVectors (v3o - amove) True True True
    (mins, maxs) <- findPusherBoundingBox updatedMove
    savePusherPosition
    movePusherToFinalPosition updatedMove
    numEdicts <- use (gameBaseGlobals.gbNumEdicts)
    done <- checkForSolidEntities (shouldSkip maxs mins) updatedMove (figureMovement forward right up) 1 numEdicts
    doPush done
  where
    doPush done
        | done = return False
        | otherwise = do
            pushedP <- use (gameBaseGlobals.gbPushedP)
            checkTriggerTouch (pushedP - 1) 0
            return True
    clampMove v =
        let temp = v * 8
            temp' = fromIntegral (truncate (if temp > 0 then temp + 0.5 else temp - 0.5) :: Int) :: Float
        in temp' * 0.125
    findPusherBoundingBox updatedMove = do
      pusher <- readRef pusherRef
      return ((pusher^.eAbsMin) + updatedMove, (pusher^.eAbsMax) + updatedMove)
    savePusherPosition = do
      pushedP <- use (gameBaseGlobals.gbPushedP)
      pusher <- readRef pusherRef
      modifyRef (Ref pushedP) (\v -> v & pEnt    .~ Just pusherRef
                                       & pOrigin .~ (pusher^.eEntityState.esOrigin)
                                       & pAngles .~ (pusher^.eEntityState.esAngles))
      maybe (return ()) (proceedSavePusherPosition pushedP) (pusher^.eClient)
      gameBaseGlobals.gbPushedP += 1
    proceedSavePusherPosition pushedP clientRef = do
        deltaAngles <- fmap (^.gcPlayerState.psPMoveState.pmsDeltaAngles) (readRef clientRef)
        modifyRef (Ref pushedP) (\v -> v & pDeltaYaw .~ fromIntegral (deltaAngles^._y))
    movePusherToFinalPosition updatedMove = do
      modifyRef pusherRef (\v -> v & eEntityState.esOrigin %~ (+ updatedMove)
                                   & eEntityState.esAngles %~ (+ amove))
      linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
      linkEntity pusherRef
    checkForSolidEntities shouldSkip' updatedMove figureMovement' idx maxIdx -- IMPROVE: old code, should be refactored
        | idx >= maxIdx = return False
        | otherwise = do
            skip <- shouldSkip' (Ref idx)
            if not skip
                then do
                    pusherMoveType <- fmap (^.eMoveType) (readRef pusherRef)
                    edictGroundEntity <- fmap (^.eGroundEntity) (readRef (Ref idx))
                    nextEntity <- if pusherMoveType == Constants.moveTypePush || edictGroundEntity == Just pusherRef
                                    then do
                                      moveEntity (Ref idx)
                                      tryMovingContactedEntity updatedMove (Ref idx)
                                      figureMovement' (Ref idx)
                                      nullifyGroundEntity (Ref idx)
                                      block <- testEntityPosition (Ref idx)
                                      if not block
                                          then do
                                              linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
                                              linkEntity (Ref idx)
                                              return True
                                          else do
                                              modifyRef (Ref idx) (\v -> v & eEntityState.esOrigin -~ updatedMove)
                                              block' <- testEntityPosition (Ref idx)
                                              if not block'
                                                  then do
                                                      gameBaseGlobals.gbPushedP -= 1
                                                      return True
                                                  else return False
                                    else return False
                    if nextEntity
                        then 
                            checkForSolidEntities shouldSkip' updatedMove figureMovement' (idx + 1) maxIdx
                        else do
                            gameBaseGlobals.gbObstacle .= Just (Ref idx)
                            pushedP <- use (gameBaseGlobals.gbPushedP)
                            moveBackEntity (pushedP - 1) 0
                            return True
                else 
                  checkForSolidEntities shouldSkip' updatedMove figureMovement' (idx + 1) maxIdx
    moveBackEntity idx minIdx
        | idx >= minIdx = do
            p <- readRef (Ref idx)
            maybe edictError (doMoveBackEntity p) (p^.pEnt)
        | otherwise = return ()
    edictError = Com.fatalError "SV.push#moveBackEntity p^.pEnt is Nothing"
    doMoveBackEntity p edictRef = do
        modifyRef edictRef (\v -> v & eEntityState.esOrigin .~ (p^.pOrigin)
                                    & eEntityState.esAngles .~ (p^.pAngles))
        clientRef <- fmap (^.eClient) (readRef edictRef)
        maybe (return ()) (updateClientDeltaAngles p) clientRef
        linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
        linkEntity edictRef
    updateClientDeltaAngles p clientRef =
        modifyRef clientRef (\v -> v & gcPlayerState.psPMoveState.pmsDeltaAngles._y .~ truncate (p^.pDeltaYaw))
    nullifyGroundEntity edictRef = do
        edict <- readRef edictRef
        when ((edict^.eGroundEntity) /= Just pusherRef) $
            modifyRef edictRef (\v -> v & eGroundEntity .~ Nothing)
    figureMovement forward right up edictRef = do
        edictOrigin <- fmap (^.eEntityState.esOrigin) (readRef edictRef)
        pusherOrigin <- fmap (^.eEntityState.esOrigin) (readRef pusherRef)
        let org = edictOrigin - pusherOrigin
            org2 = V3 (dot org forward) (dot org right) (dot org up)
            move2 = org2 - org
        modifyRef edictRef (\v -> v & eEntityState.esOrigin +~ move2)
    tryMovingContactedEntity updatedMove edictRef = do
        modifyRef edictRef (\v -> v & eEntityState.esOrigin +~ updatedMove)
        clientRef <- fmap (^.eClient) (readRef edictRef)
        maybe (return ()) updateClientDeltaAngles2 clientRef
    updateClientDeltaAngles2 clientRef =
        modifyRef clientRef (\v -> v & gcPlayerState.psPMoveState.pmsDeltaAngles._y +~ (truncate (amove^._y)))
    moveEntity edictRef = do
        pushedP <- use (gameBaseGlobals.gbPushedP)
        entityState <- fmap (^.eEntityState) (readRef edictRef)
        modifyRef (Ref pushedP) (\v -> v & pEnt    .~ Just edictRef
                                         & pOrigin .~ (entityState^.esOrigin)
                                         & pAngles .~ (entityState^.esAngles))
        gameBaseGlobals.gbPushedP += 1
    shouldSkip maxs mins edictRef = do
      edict <- readRef edictRef
      linked <- isLinkedAnywhere (edict^.eArea)
      checkShouldSkip maxs mins edictRef edict linked
    checkShouldSkip maxs mins edictRef edict linked
        | not (edict^.eInUse) = return True
        | any (== (edict^.eMoveType)) [Constants.moveTypePush, Constants.moveTypeStop, Constants.moveTypeNone, Constants.moveTypeNoClip] = return True
        | not linked = return True
        | (edict^.eGroundEntity) /= Just edictRef = do
            -- see if the ent needs to be tested
            let absmin = edict^.eAbsMin
                absmax = edict^.eAbsMax
            if absmin^._x >= maxs^._x || absmin^._y >= maxs^._y || absmin^._z >= maxs^._z ||
               absmax^._x <= mins^._x || absmax^._y <= mins^._y || absmax^._z <= mins^._z
                then return True
                else fmap not (testEntityPosition edictRef)
        | otherwise = return False
    isLinkedAnywhere linkRef = do
      link <- readRef linkRef
      return (isJust (link^.lPrev))
    checkTriggerTouch idx minIdx
        | idx >= minIdx = do
            edictRef <- fmap (^.pEnt) (readRef (Ref idx))
            maybe edictError2 GameBase.touchTriggers edictRef
        | otherwise = return ()
    edictError2 = Com.fatalError "SV.push#checkTriggerTouch edictRef is Nothing"

moveStep :: Ref EdictT -> V3 Float -> Bool -> Quake Bool
moveStep edictRef move relink = do
    edict <- readRef edictRef
    doMoveStep edictRef edict move relink

doMoveStep :: Ref EdictT -> EdictT -> V3 Float -> Bool -> Quake Bool
doMoveStep edictRef edict move relink
    | (edict^.eFlags) .&. (Constants.flSwim .|. Constants.flFly) /= 0 = -- flying monsters don't step up
        -- try one move with vertical motion, then one without
        moveStepFlyingMonster edictRef move relink 0 2
    | otherwise = do
        trace <- use (gameBaseGlobals.gbGameImport.giTrace)
        traceT <- trace newOrg
                        (Just (edict^.eMins))
                        (Just (edict^.eMaxs))
                        end
                        (Just edictRef)
                        Constants.maskMonsterSolid
        checkAllSolid traceT
            >>= checkStartSolid traceT
            >>= checkWaterLevel
            >>= checkFraction
            >>= checkBottom
            >>= moveIsOk
  where
    stepSize
        | (edict^.eMonsterInfo.miAIFlags) .&. Constants.aiNoStep == 0 = fromIntegral Constants.stepSize
        | otherwise                                                   = 1
    newOrg = ((edict^.eEntityState.esOrigin) + move) & _z +~ stepSize
    end = newOrg & _z -~ (stepSize * 2)
    checkAllSolid traceT
        | traceT^.tAllSolid = return (Just False)
        | otherwise         = return Nothing
    checkStartSolid traceT Nothing
        | traceT^.tStartSolid = do
            trace <- use (gameBaseGlobals.gbGameImport.giTrace)
            traceT' <- trace (newOrg & _z -~ stepSize)
                             (Just (edict^.eMins))
                             (Just (edict^.eMaxs))
                             end
                             (Just edictRef)
                             Constants.maskMonsterSolid
            return (if (traceT'^.tAllSolid) || (traceT'^.tStartSolid) then (traceT', Just False) else (traceT', Nothing))
        | otherwise = return (traceT, Nothing)
    checkStartSolid traceT result = return (traceT, result)
    checkWaterLevel (traceT, Nothing)
        | (edict^.eWaterLevel) == 0 = do
            pointContents <- use (gameBaseGlobals.gbGameImport.giPointContents)
            contents <- pointContents ((traceT^.tEndPos) & _z +~ (edict^.eMins._z + 1))
            return (if contents .&. Constants.maskWater /= 0 then (traceT, Just False) else (traceT, Nothing))
        | otherwise = return (traceT, Nothing)
    checkWaterLevel result = return result
    checkFraction (traceT, Nothing)
        | (traceT^.tFraction) == 1 && (edict^.eFlags) .&. Constants.flPartialGround /= 0 = do
            modifyRef edictRef (\v -> v & eEntityState.esOrigin +~ move)
            doRelink
            modifyRef edictRef (\v -> v & eGroundEntity .~ Nothing)
            return (traceT, Just True)
        | (traceT^.tFraction) == 1 = return (traceT, Just False) -- walked off an edge
        | otherwise = return (traceT, Nothing)
    checkFraction result = return result
    checkBottom (traceT, Nothing) = do
        modifyRef edictRef (\v -> v & eEntityState.esOrigin .~ (traceT^.tEndPos))
        hasBottom <- M.checkBottom edictRef
        doCheckBottom hasBottom traceT
    checkBottom result = return result
    doCheckBottom hasBottom traceT
        | (not hasBottom) && (edict^.eFlags) .&. Constants.flPartialGround /= 0 = do
            doRelink
            return (traceT, Just True)
        | not hasBottom = do
            modifyRef edictRef (\v -> v & eEntityState.esOrigin .~ (edict^.eEntityState.esOrigin))
            return (traceT, Just False)
        | otherwise = return (traceT, Nothing)
    moveIsOk (traceT, Nothing) = do
        when ((edict^.eFlags) .&. Constants.flPartialGround /= 0) $
            modifyRef edictRef (\v -> v & eFlags %~ (.&. (complement Constants.flPartialGround)))
        updateGroundEntity traceT (traceT^.tEnt)
        doRelink
        return True
    moveIsOk (_, Just result) = return result
    doRelink
        | relink = do
            linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
            linkEntity edictRef
            GameBase.touchTriggers edictRef
        | otherwise = return ()
    updateGroundEntity _ Nothing = Com.fatalError "SV.moveStep traceT^.tEnt is Nothing"
    updateGroundEntity traceT (Just entRef) = do
        ent <- readRef entRef
        modifyRef edictRef (\v -> v & eGroundEntity .~ traceT^.tEnt
                                    & eGroundEntityLinkCount .~ (ent^.eLinkCount))

moveStepFlyingMonster :: Ref EdictT -> V3 Float -> Bool -> Int -> Int -> Quake Bool
moveStepFlyingMonster edictRef move relink idx maxIdx
    | idx >= maxIdx = return False
    | otherwise = do
        edict <- readRef edictRef
        newOrg <- getNewOrg edict (edict^.eEnemy)
        trace <- use (gameBaseGlobals.gbGameImport.giTrace)
        traceT <- trace (edict^.eEntityState.esOrigin)
                        (Just (edict^.eMins))
                        (Just (edict^.eMaxs))
                        newOrg
                        (Just edictRef)
                        Constants.maskMonsterSolid
        checkFlyMonsters edict traceT
            >>= checkSwimMonsters edict traceT
            >>= checkFraction traceT
            >>= checkEnemy edict
  where
    defaultNewOrg edict = (edict^.eEntityState.esOrigin) + move
    getNewOrg edict Nothing = return (defaultNewOrg edict)
    getNewOrg edict (Just enemyRef)
        | idx == 0 = do
            when (isNothing (edict^.eGoalEntity)) $
                modifyRef edictRef (\v -> v & eGoalEntity .~ (edict^.eEnemy))
            goalEntity <- readGoalEntity (edict^.eGoalEntity) enemyRef
            return (doGetNewOrg edict goalEntity ((edict^.eEntityState.esOrigin._z) - (goalEntity^.eEntityState.esOrigin._z)))
        | otherwise = return (defaultNewOrg edict)
    readGoalEntity Nothing enemyRef = readRef enemyRef
    readGoalEntity (Just goalEntityRef) _ = readRef goalEntityRef
    doGetNewOrg edict goalEntity dz
        | isJust (goalEntity^.eClient) = getClientNewOrg (defaultNewOrg edict) edict dz
        | otherwise = getNonClientNewOrg (defaultNewOrg edict) dz
    getClientNewOrg newOrg edict dz
        | dz > 40 = newOrg & _z -~ 8
        | (not (((edict^.eFlags) .&. Constants.flSwim /= 0) && ((edict^.eWaterLevel) < 2))) && (dz < 30) = newOrg & _z +~ 8
        | otherwise = newOrg
    getNonClientNewOrg newOrg dz
        | dz > 8    = newOrg & _z -~ 8
        | dz > 0    = newOrg & _z -~ dz
        | dz < (-8) = newOrg & _z +~ 8
        | otherwise = newOrg & _z +~ dz
    checkFlyMonsters edict traceT
        | ((edict^.eFlags) .&. Constants.flFly /= 0) && (edict^.eWaterLevel) == 0 = do
            pointContents <- use (gameBaseGlobals.gbGameImport.giPointContents)
            contents <- pointContents ((traceT^.tEndPos) & _z +~ (edict^.eMins._z + 1))
            return (if (contents .&. Constants.maskWater /= 0) then Just False else Nothing)
        | otherwise = return Nothing
    checkSwimMonsters edict traceT Nothing
        | ((edict^.eFlags) .&. Constants.flSwim /= 0) && (edict^.eWaterLevel) < 2 = do
            pointContents <- use (gameBaseGlobals.gbGameImport.giPointContents)
            contents <- pointContents ((traceT^.tEndPos) & _z +~ (edict^.eMins._z + 1))
            return (if (contents .&. Constants.maskWater == 0) then Just False else Nothing)
        | otherwise = return Nothing
    checkSwimMonsters _ _ result = return result
    checkFraction traceT Nothing = do
        modifyRef edictRef (\v -> v & eEntityState.esOrigin .~ (traceT^.tEndPos))
        when relink $ do
            linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
            linkEntity edictRef
            GameBase.touchTriggers edictRef
        return (Just True)
    checkFraction _ result = return result
    checkEnemy edict Nothing
        | isNothing (edict^.eEnemy) = return False
        | otherwise = moveStepFlyingMonster edictRef move relink (idx + 1) maxIdx
    checkEnemy _ (Just result) = return result

addRotationalFriction :: Ref EdictT -> Quake ()
addRotationalFriction = error "SV.addRotationalFriction" -- TODO

addGravity :: Ref EdictT -> Quake ()
addGravity edictRef = do
    edict <- readRef edictRef
    gravity <- fmap (^.cvValue) svGravityCVar
    modifyRef edictRef (\v -> v & eVelocity._z -~ (edict^.eGravity) * gravity * Constants.frameTime)

flyMove :: Ref EdictT -> Float -> Int -> Quake Int
flyMove edictRef time mask = do
    edict <- readRef edictRef
    modifyRef edictRef (\v -> v & eGroundEntity .~ Nothing)
    doFlyMove edictRef (edict^.eVelocity) (edict^.eVelocity) (V.replicate 6 (V3 0 0 0)) time mask 0 0 0 4

-- TODO: REFACTOR, old code, too tired to fix it now
doFlyMove :: Ref EdictT -> V3 Float -> V3 Float -> V.Vector (V3 Float) -> Float -> Int -> Int -> Int -> Int -> Int -> Quake Int
doFlyMove edictRef primalVelocity originalVelocity planes timeLeft mask numPlanes blockedMask idx maxIdx
    | idx >= maxIdx = return blockedMask
    | otherwise = do
        traceT <- getTraceT timeLeft
        proceedFlyMove traceT
  where
    proceedFlyMove traceT
        | (traceT^.tAllSolid) = do -- entity is trapped in another solid
            v3o <- use (globals.gVec3Origin)
            modifyRef edictRef (\v -> v & eVelocity .~ v3o)
            return 3
        | otherwise = do
            (numPlanes', originalVelocity') <- checkCoveredDistance traceT
            if (traceT^.tFraction) == 1 -- moved the entire distance
              then
                return blockedMask
              else do
                let Just hitRef = traceT^.tEnt
                blockedMask' <- if traceT^.tPlane.cpNormal._z > 0.7
                                  then do
                                    readRef hitRef >>= \hit -> do
                                      when ((hit^.eSolid) == Constants.solidBsp) $ do
                                        modifyRef edictRef (\v -> v & eGroundEntity .~ (traceT^.tEnt)
                                                                       & eGroundEntityLinkCount .~ (hit^.eLinkCount))
                                      return (blockedMask .|. 1) -- floor
                                  else
                                    return $ if (traceT^.tPlane.cpNormal._z) == 0
                                               then blockedMask .|. 2 -- step
                                               else blockedMask
                -- run the impact function
                impact edictRef traceT
                inUse <- readRef edictRef >>= \e -> return (e^.eInUse)
                if not inUse -- removed by the impact function
                  then
                    return blockedMask'
                  else do
                    let timeLeft' = timeLeft - timeLeft * (traceT^.tFraction)
                    -- cliped to another plane
                    if numPlanes' >= Constants.maxClipPlanes -- this shouldn't really happen
                      then do
                        use (globals.gVec3Origin) >>= \v3o ->
                          modifyRef edictRef (\v -> v & eVelocity .~ v3o)
                        return 3
                      else do
                        let planes' = planes V.// [(numPlanes', traceT^.tPlane.cpNormal)]
                            numPlanes'' = numPlanes' + 1
                        -- modify original_velocity so it parallels all of the clip planes
                        let (i, newVelocity) = modifyOriginVelocity planes' originalVelocity' 0 numPlanes'' (V3 0 0 0)
                        if i /= numPlanes'' -- go along this plane
                          then do
                            modifyRef edictRef (\v -> v & eVelocity .~ newVelocity)
                            -- if original velocity is against the original velocity, stop dead
                            -- to avoid tiny occilations in sloping corners
                            if newVelocity `dot` primalVelocity <= 0
                              then do
                                use (globals.gVec3Origin) >>= \v3o ->
                                  modifyRef edictRef (\v -> v & eVelocity .~ v3o)
                                return blockedMask'
                              else
                                doFlyMove edictRef primalVelocity originalVelocity' planes' timeLeft' mask numPlanes'' blockedMask' (idx + 1) maxIdx
                          else do -- go along the crease
                            if numPlanes'' /= 2
                              then do
                                use (globals.gVec3Origin) >>= \v3o ->
                                  modifyRef edictRef (\v -> v & eVelocity .~ v3o)
                                return 7
                              else do
                                entVelocity <- readRef edictRef >>= \e -> return (e^.eVelocity)
                                let dir = (planes' V.! 0) `cross` (planes' V.! 1)
                                    d = dir `dot` entVelocity
                                modifyRef edictRef (\v -> v & eVelocity .~ fmap (* d) dir)
                                -- if original velocity is against the original velocity, stop dead
                                -- to avoid tiny occilations in sloping corners
                                if newVelocity `dot` primalVelocity <= 0
                                  then do
                                    use (globals.gVec3Origin) >>= \v3o ->
                                      modifyRef edictRef (\v -> v & eVelocity .~ v3o)
                                    return blockedMask'
                                  else
                                    doFlyMove edictRef primalVelocity originalVelocity' planes' timeLeft' mask numPlanes'' blockedMask' (idx + 1) maxIdx
    getTraceT timeLeft = do
        edict <- readRef edictRef
        trace <- use (gameBaseGlobals.gbGameImport.giTrace)
        trace (edict^.eEntityState.esOrigin)
              (Just (edict^.eMins))
              (Just (edict^.eMaxs))
              ((edict^.eEntityState.esOrigin) + fmap (* timeLeft) (edict^.eVelocity))
              (Just edictRef)
              mask
    checkCoveredDistance traceT
        | (traceT^.tFraction) > 0 = do -- actually covered some distance
            modifyRef edictRef (\v -> v & eEntityState.esOrigin .~ (traceT^.tEndPos))
            v <- fmap (^.eVelocity) (readRef edictRef)
            return (0, v)
        | otherwise =
            return (numPlanes, originalVelocity)
    modifyOriginVelocity planes originalVelocity idx maxIdx newVelocity
        | idx >= maxIdx = (idx, newVelocity)
        | otherwise =
            let (_, newVelocity') = GameBase.clipVelocity originalVelocity (planes V.! idx) 1
                j = checkPlanes planes newVelocity' idx 0 maxIdx
            in if j == maxIdx
                   then (idx, newVelocity')
                   else modifyOriginVelocity planes originalVelocity (idx + 1) maxIdx newVelocity'
    checkPlanes planes newVelocity i idx maxIdx
        | idx >= maxIdx = idx
        | otherwise =
            if idx /= i && (planes V.! idx) /= (planes V.! i)
                then if newVelocity `dot` (planes V.! idx) < 0 -- not ok
                         then idx
                         else checkPlanes planes newVelocity i (idx + 1) maxIdx
                else checkPlanes planes newVelocity i (idx + 1) maxIdx

impact :: Ref EdictT -> TraceT -> Quake ()
impact edictRef traceT =
    maybe entError doImpact (traceT^.tEnt)
  where
    entError = Com.fatalError "SV.impact traceT^.tEnt is Nothing"
    doImpact traceRef = do
        edict <- readRef edictRef
        traceEdict <- readRef traceRef
        edictImpact traceRef edict (edict^.eTouch)
        traceImpact traceRef traceEdict (traceEdict^.eTouch)
    edictImpact _ _ Nothing = return ()
    edictImpact traceRef edict (Just edictTouch)
        | (edict^.eSolid) /= Constants.solidNot =
            entTouch edictTouch edictRef traceRef (traceT^.tPlane) (traceT^.tSurface)
        | otherwise = return ()
    traceImpact _ _ Nothing = return ()
    traceImpact traceRef traceEdict (Just traceTouch)
        | (traceEdict^.eSolid) /= Constants.solidNot = do
            dummyPlane <- use (gameBaseGlobals.gbDummyPlane)
            entTouch traceTouch traceRef edictRef dummyPlane Nothing
        | otherwise = return ()

pushEntity :: Ref EdictT -> V3 Float -> Quake TraceT
pushEntity edictRef pushV3 = do
    edict <- readRef edictRef
    traceT <- tryToPush (edict^.eEntityState.esOrigin) ((edict^.eEntityState.esOrigin) + pushV3)
    inUse <- fmap (^.eInUse) (readRef edictRef)
    when inUse $
        GameBase.touchTriggers edictRef
    return traceT
  where
    tryToPush start end = do
        edict <- readRef edictRef
        gameImport <- use (gameBaseGlobals.gbGameImport)
        traceT <- (gameImport^.giTrace) start
                                        (Just (edict^.eMins))
                                        (Just (edict^.eMaxs))
                                        end
                                        (Just edictRef)
                                        (mask (edict^.eClipMask))
        modifyRef edictRef (\v -> v & eEntityState.esOrigin .~ (traceT^.tEndPos))
        (gameImport^.giLinkEntity) edictRef
        checkTrace start end traceT
    checkTrace start end traceT
        | (traceT^.tFraction) /= 1.0 = do
            impact edictRef traceT
            edict <- readRef edictRef
            maybe (traceError traceT) (\traceRef -> checkPushedEntity start end traceT edict =<< readRef traceRef) (traceT^.tEnt)
        | otherwise = return traceT
    traceError traceT = do
        Com.fatalError "SV.pushEntity#checkTrace traceT^.tEnt is Nothing"
        return traceT
    checkPushedEntity start end traceT edict traceEdict
        | not (traceEdict^.eInUse) && (edict^.eInUse) = do
            modifyRef edictRef (\v -> v & eEntityState.esOrigin .~ start)
            linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
            linkEntity edictRef
            tryToPush start end
        | otherwise = return traceT
    mask clipMask
        | clipMask /= 0 = clipMask
        | otherwise = Constants.maskSolid

testEntityPosition :: Ref EdictT -> Quake Bool
testEntityPosition edictRef = do
    edict <- readRef edictRef
    let mask = if (edict^.eClipMask) /= 0 then edict^.eClipMask else Constants.maskSolid
    trace <- use (gameBaseGlobals.gbGameImport.giTrace)
    traceT <- trace (edict^.eEntityState.esOrigin)
                    (Just (edict^.eMins))
                    (Just (edict^.eMaxs))
                    (edict^.eEntityState.esOrigin)
                    (Just edictRef)
                    mask
    return (traceT^.tStartSolid)
