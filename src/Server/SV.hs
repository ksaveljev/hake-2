module Server.SV
    ( moveStep
    , physicsNoClip
    , physicsNone
    , physicsPusher
    , physicsStep
    , physicsToss
    ) where

import           Control.Lens          (use, (^.), (.=), (&), (.~), (+~), (-~), (%~))
import           Control.Monad         (when, void, unless)
import           Data.Bits             ((.&.), (.|.))
import           Data.Maybe            (isJust, isNothing)
import qualified Data.Vector           as V
import           Linear                (V3(..), cross, dot, _x, _y, _z)

import qualified Constants
import           Game.CPlaneT
import           Game.CVarT
import           Game.EdictT
import           Game.EntityStateT
import           Game.LevelLocalsT
import           Game.TraceT
import qualified QCommon.Com           as Com
import           QCommon.CVarVariables
import           QuakeRef
import           QuakeState
import           Types

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
physicsToss = error "SV.physicsToss" -- TODO

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
push = error "SV.push" -- TODO

moveStep :: Ref EdictT -> V3 Float -> Bool -> Quake Bool
moveStep = error "SV.moveStep" -- TODO

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
