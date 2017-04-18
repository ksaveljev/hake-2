module Server.SV
    ( moveStep
    , physicsNoClip
    , physicsNone
    , physicsPusher
    , physicsStep
    , physicsToss
    ) where

import           Control.Lens      (use, (^.), (.=), (.~), (+~), (&))
import           Control.Monad     (when, void, unless)
import           Data.Bits         ((.&.))
import           Linear            (V3(..), _x, _y, _z)

import qualified Constants
import           Game.EdictT
import           Game.EntityStateT
import           Game.LevelLocalsT
import qualified QCommon.Com       as Com
import           QuakeRef
import           QuakeState
import           Types

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
        error "SV.physicsPusher" -- TODO

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
physicsStep = error "SV.physicsStep" -- TODO

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