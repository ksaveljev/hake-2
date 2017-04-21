module Client.M
    ( checkBottom
    , checkGround
    , dropToFloor
    , flyCheck
    , walkMove
    ) where

import           Control.Lens      (use, (^.), (&), (.~), (-~), (+~), (%~))
import           Control.Monad     (when, unless)
import           Data.Bits         (complement, (.&.), (.|.))
import           Data.Maybe        (isNothing)
import           Linear            (V3(..), _z)

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

checkBottom :: Ref EdictT -> Quake Bool
checkBottom = error "M.checkBottom" -- TODO
