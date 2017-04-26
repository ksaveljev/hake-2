module Client.M
    ( catagorizePosition
    , checkBottom
    , checkGround
    , dropToFloor
    , flyCheck
    , moveFrame
    , setEffects
    , walkMove
    , worldEffects
    ) where

import           Control.Lens      (use, (^.), (+=), (&), (.~), (-~), (+~), (%~))
import           Control.Monad     (when, unless)
import           Data.Bits         (complement, (.&.), (.|.))
import           Data.Maybe        (isNothing)
import           Linear            (V3(..), _x, _y, _z)

import qualified Constants
import           Game.CPlaneT
import           Game.EdictT
import           Game.EntityStateT
import           Game.LevelLocalsT
import           Game.TraceT
import qualified QCommon.Com       as Com
import           QuakeRef
import           QuakeState
import qualified Server.SV         as SV
import           Types
import qualified Util.Lib          as Lib

checkGround :: Ref EdictT -> Quake ()
checkGround edictRef = do
    edict <- readRef edictRef
    unless ((edict^.eFlags) .&. (Constants.flSwim .|. Constants.flFly) /= 0) $ do
        setGroundEntity edict
  where
    setGroundEntity edict
        | (edict^.eVelocity._z) > 100 =
            modifyRef edictRef (\v -> v & eGroundEntity .~ Nothing)
        | otherwise = do
            trace <- use (gameBaseGlobals.gbGameImport.giTrace)
            traceT <- trace (edict^.eEntityState.esOrigin) 
                           (Just (edict^.eMins))
                           (Just (edict^.eMaxs))
                           ((edict^.eEntityState.esOrigin) & _z -~ 0.25) -- if the hull point one-quarter unit down is solid the entity is on ground
                           (Just edictRef)
                           Constants.maskMonsterSolid
            checkSteepness traceT
    checkSteepness traceT
        | (traceT^.tPlane.cpNormal._z) < 0.7 && not (traceT^.tStartSolid) =
            modifyRef edictRef (\v -> v & eGroundEntity .~ Nothing)
        | otherwise = do
            when (not (traceT^.tStartSolid) && not (traceT^.tAllSolid)) $ do
                maybe traceEntError (doSetGroundEntity traceT) (traceT^.tEnt)
    traceEntError = Com.fatalError "M.checkGround traceT^.tEnt is Nothing"
    doSetGroundEntity traceT traceEntRef = do
        traceEnt <- readRef traceEntRef
        modifyRef edictRef (\v -> v & eEntityState.esOrigin .~ (traceT^.tEndPos)
                                    & eGroundEntity .~ (traceT^.tEnt)
                                    & eGroundEntityLinkCount .~ (traceEnt^.eLinkCount)
                                    & eVelocity._z .~ 0)

dropToFloor :: EntThink
dropToFloor = EntThink "m_drop_to_floor" $ \edictRef -> do
    modifyRef edictRef (\v -> v & eEntityState.esOrigin._z +~ 1)
    edict <- readRef edictRef
    trace <- use (gameBaseGlobals.gbGameImport.giTrace)
    traceT <- trace (edict^.eEntityState.esOrigin)
                    (Just (edict^.eMins))
                    (Just (edict^.eMaxs))
                    ((edict^.eEntityState.esOrigin) & _z -~ 256)
                    (Just edictRef)
                    Constants.maskMonsterSolid
    doDropToFloor edictRef traceT
  where
    doDropToFloor edictRef traceT
        | (traceT^.tFraction) == 1 || (traceT^.tAllSolid) =
            return True
        | otherwise = do
            modifyRef edictRef (\v -> v & eEntityState.esOrigin .~ (traceT^.tEndPos))
            linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
            linkEntity edictRef
            checkGround edictRef
            catagorizePosition edictRef
            return True

walkMove :: Ref EdictT -> Float -> Float -> Quake Bool
walkMove edictRef yaw dist = do
    edict <- readRef edictRef
    doWalkMove edict
  where
    doWalkMove edict
        | isNothing (edict^.eGroundEntity) && ((edict^.eFlags) .&. (Constants.flFly .|. Constants.flSwim) == 0) =
            return False
        | otherwise = do
            let yaw' = yaw * pi * 2 / 360
                move = V3 ((cos yaw') * dist) ((sin yaw') * dist) 0
            SV.moveStep edictRef move True

catagorizePosition :: Ref EdictT -> Quake ()
catagorizePosition edictRef = do
    edict <- readRef edictRef
    pointContents <- use (gameBaseGlobals.gbGameImport.giPointContents)
    let point = (edict^.eEntityState.esOrigin) & _z +~ (edict^.eMins._z) + 1
    cont <- pointContents point
    doCatagorizePosition point cont
  where
    doCatagorizePosition point cont
        | cont .&. Constants.maskWater == 0 =
            modifyRef edictRef (\v -> v & eWaterLevel .~ 0
                                        & eWaterType .~ 0)
        | otherwise = do
            modifyRef edictRef (\v -> v & eWaterLevel .~ 1
                                        & eWaterType .~ cont)
            let point' = (point & _z +~ 26)
            pointContents <- use (gameBaseGlobals.gbGameImport.giPointContents)
            cont' <- pointContents point'
            unless (cont' .&. Constants.maskWater == 0) $ do
                modifyRef edictRef (\v -> v & eWaterLevel .~ 2)
                let point'' = point' & _z +~ 22
                cont'' <- pointContents point''
                when (cont'' .&. Constants.maskWater /= 0) $
                    modifyRef edictRef (\v -> v & eWaterLevel .~ 3)

flyCheck :: EntThink
flyCheck = EntThink "m_fly_check" $ \edictRef -> do
    edict <- readRef edictRef
    f <- Lib.randomF
    unless ((edict^.eWaterLevel /= 0) || f > 0.5) $ do
        levelTime <- use (gameBaseGlobals.gbLevel.llTime)
        nf <- Lib.randomF
        modifyRef edictRef (\v -> v & eThink .~ Just fliesOn
                                    & eNextThink .~ levelTime + 5 + 10 * nf)
    return True

fliesOn :: EntThink
fliesOn = EntThink "m_flies_on" $ \edictRef -> do
    edict <- readRef edictRef
    unless ((edict^.eWaterLevel) /= 0) $ do
        levelTime <- use (gameBaseGlobals.gbLevel.llTime)
        soundIndex <- use (gameBaseGlobals.gbGameImport.giSoundIndex)
        soundIdx <- soundIndex (Just "infantry/inflies1.wav")
        modifyRef edictRef (\v -> v & eEntityState.esEffects %~ (.|. Constants.efFlies)
                                    & eEntityState.esSound .~ soundIdx
                                    & eThink .~ Just fliesOff
                                    & eNextThink .~ levelTime + 60)
    return True

fliesOff :: EntThink
fliesOff = EntThink "m_fliesoff" $ \edictRef -> do
    modifyRef edictRef (\v -> v & eEntityState.esEffects %~ (.&. (complement Constants.efFlies))
                                & eEntityState.esSound .~ 0)
    return True

-- TODO: old implementation, needs some refactoring
checkBottom :: Ref EdictT -> Quake Bool
checkBottom edictRef = do
    edict <- readRef edictRef
    let mins = (edict^.eEntityState.esOrigin) + (edict^.eMins)
        maxs = (edict^.eEntityState.esOrigin) + (edict^.eMaxs)
    -- if all of the points under the corners are solid world, don't bother
    -- with the tougher checks
    -- the corners must be within 16 of the midpoint
    done <- doChecks ((mins^._z) - 1) mins maxs 0 2 0 2
    maybe gotOutEasy return done
  where
    doChecks c mins maxs x maxX y maxY
        | x >= maxX = return Nothing
        | y >= maxY = doChecks c mins maxs (x + 1) maxX 0 maxY
        | otherwise = do
            let a = if x /= 0 then maxs^._x else mins^._x
                b = if y /= 0 then maxs^._y else mins^._y
                start = V3 a b c
            gameImport <- use (gameBaseGlobals.gbGameImport)
            contents <- (gameImport^.giPointContents) start
            if contents /= Constants.contentsSolid
                then do
                    gameBaseGlobals.gbCNo += 1
                    -- check it for real
                    let a' = ((mins^._x) + (maxs^._x)) * 0.5
                        b' = ((mins^._y) + (maxs^._y)) * 0.5
                        start' = V3 a' b' (mins^._z)
                        stop' = V3 a' b' ((mins^._z) - 2 * (fromIntegral Constants.stepSize))
                    v3o <- use (globals.gVec3Origin)
                    traceT <- (gameImport^.giTrace) start' (Just v3o) (Just v3o) stop' (Just edictRef) Constants.maskMonsterSolid
                    if (traceT^.tFraction) == 1
                        then
                            return (Just False)
                        else do
                            let mid = traceT^.tEndPos._z
                                bottom = traceT^.tEndPos._z
                            -- the corners must be withing 16 of the midpoint
                            done <- checkCorners mins maxs start' stop' mid bottom 0 2 0 2
                            case done of
                                Just _ -> return done
                                Nothing -> do
                                    gameBaseGlobals.gbCYes += 1
                                    return (Just True)
                else
                    doChecks c mins maxs x maxX (y + 1) maxY
    checkCorners mins maxs start stop mid bottom x maxX y maxY
        | x >= maxX = return Nothing
        | y >= maxY = checkCorners mins maxs start stop mid bottom (x + 1) maxX 0 maxY
        | otherwise = do
            let a = if x /= 0 then maxs^._x else mins^._x
                b = if y /= 0 then maxs^._y else mins^._y
                start' = V3 a b (start^._z)
                stop' = V3 a b (stop^._z)
            v3o <- use (globals.gVec3Origin)
            trace <- use (gameBaseGlobals.gbGameImport.giTrace)
            traceT <- trace start' (Just v3o) (Just v3o) stop' (Just edictRef) Constants.maskMonsterSolid
            let bottom' | (traceT^.tFraction) /= 1 && (traceT^.tEndPos._z) > bottom = traceT^.tEndPos._z
                        | otherwise                                                 = bottom
            if (traceT^.tFraction) == 1 || mid - (traceT^.tEndPos._z) > fromIntegral Constants.stepSize
                then return (Just False)
                else checkCorners mins maxs start' stop' mid bottom' x maxX (y + 1) maxY
    gotOutEasy = do
        gameBaseGlobals.gbCYes += 1
        return True

setEffects :: Ref EdictT -> Quake ()
setEffects = error "M.setEffects" -- TODO

worldEffects :: Ref EdictT -> Quake ()
worldEffects = error "M.worldEffects" -- TODO

moveFrame :: Ref EdictT -> Quake ()
moveFrame = error "M.moveFrame" -- TODO
