{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Server.SV where

import Control.Lens (use, preuse, ix, (^.), (.=), (+=), (-=), zoom)
import Control.Monad (unless, when, void, liftM)
import Data.Bits ((.&.))
import Data.Maybe (isJust, fromJust, isNothing)
import Linear (V3, _x, _y, _z)

import Quake
import QuakeState
import CVarVariables
import qualified Constants
import qualified Game.GameBase as GameBase
import qualified QCommon.Com as Com
import qualified Server.SVGame as SVGame

{-
- 
- Bmodel objects don't interact with each other, but push all box objects.
-}
physicsPusher :: EdictReference -> Quake ()
physicsPusher er@(EdictReference edictIdx) = do
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

    -- if not a team captain, so movement will be handled elsewhere
    unless ((edict^.eFlags) .&. Constants.flTeamSlave /= 0) $ do
      -- make sure all team slaves can move before commiting
      -- any moves or calling any think functions
      -- if the move is blocked, all moved objects will be backed out
      -- retry:
      gameBaseGlobals.gbPushedP .= 0
      finalEdict <- pushTeamChain (Just er)

      pushedP <- use $ gameBaseGlobals.gbPushedP

      when (pushedP > Constants.maxEdicts) $
        SVGame.pfError2 Constants.errFatal "pushed_p > &pushed[MAX_EDICTS], memory corrupted"

      if isJust finalEdict
        then do
          -- the move failed, bump all nextthink times and back out moves
          backOutTeamChain (Just er)
          let Just (EdictReference blockedIdx) = finalEdict
          Just blockedEdict <- preuse $ gameBaseGlobals.gbGEdicts.ix blockedIdx

          when (isJust (blockedEdict^.eEdictAction.eaBlocked)) $ do
            obstacle <- use $ gameBaseGlobals.gbObstacle
            blocked (fromJust $ blockedEdict^.eEdictAction.eaBlocked) (fromJust finalEdict) (fromJust obstacle)

        else
          -- the move succeeded, so call all think functions
          thinkTeamChain (Just er)

  where pushTeamChain :: Maybe EdictReference -> Quake (Maybe EdictReference)
        pushTeamChain Nothing = return Nothing
        pushTeamChain (Just chain@(EdictReference chainIdx)) = do
          Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix chainIdx
          
          let velocity = edict^.eEdictPhysics.eVelocity
              avelocity = edict^.eEdictPhysics.eAVelocity

          if (velocity^._x) /= 0 || (velocity^._y) /= 0 || (velocity^._z) /= 0 || (avelocity^._x) /= 0 || (avelocity^._y) /= 0 || (avelocity^._z) /= 0
            then do
              -- object is moving
              let move = fmap (* Constants.frameTime) velocity
                  amove = fmap (* Constants.frameTime) avelocity

              pushed <- push chain move amove
              if pushed
                then pushTeamChain (edict^.eEdictOther.eoTeamChain)
                else return (Just chain)

            else pushTeamChain (edict^.eEdictOther.eoTeamChain)

        thinkTeamChain :: Maybe EdictReference -> Quake ()
        thinkTeamChain Nothing = return ()
        thinkTeamChain (Just chain@(EdictReference chainIdx)) = do
          Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix chainIdx

          void $ runThink chain

          thinkTeamChain (edict^.eEdictOther.eoTeamChain)

        backOutTeamChain :: Maybe EdictReference -> Quake ()
        backOutTeamChain Nothing = return ()
        backOutTeamChain (Just (EdictReference chainIdx)) = do
          Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix chainIdx

          when ((edict^.eEdictAction.eaNextThink) > 0) $
            gameBaseGlobals.gbGEdicts.ix chainIdx.eEdictAction.eaNextThink += Constants.frameTime

          backOutTeamChain (edict^.eEdictOther.eoTeamChain)

-- Non moving objects can only think
physicsNone :: EdictReference -> Quake ()
physicsNone = void . runThink -- regular thinking

physicsNoClip :: EdictReference -> Quake ()
physicsNoClip _ = io (putStrLn "SV.physicsNoClip") >> undefined -- TODO

physicsStep :: EdictReference -> Quake ()
physicsStep _ = io (putStrLn "SV.physicsStep") >> undefined -- TODO

-- Toss, bounce, and fly movement. When onground, do nothing
physicsToss :: EdictReference -> Quake ()
physicsToss er@(EdictReference edictIdx) = do
    -- regular thinking
    void $ runThink er

    Just edictFlags <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eFlags

    -- if not a team captain, so movement will be handled elsewhere
    unless (edictFlags .&. Constants.flTeamSlave /= 0) $ do
      onGround <- checkGroundEntity

      -- if onground, return without moving
      unless onGround $ do
        Just oldOrigin <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esOrigin

        checkVelocity er

        -- add gravity
        Just moveType <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eMoveType
        addGravityBasedOnMoveType moveType

        -- move angles
        moveAngles

        -- move origin
        move <- moveOrigin
        trace <- pushEntity er move

        Just inUse <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eInUse

        when inUse $ do
          when (trace^.tFraction < 1) $ do
            let backoff = if moveType == Constants.moveTypeBounce
                            then 1.5
                            else 1

            Just velocity <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictPhysics.eVelocity
            void $ GameBase.clipVelocity velocity (trace^.tPlane.cpNormal) (gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictPhysics.eVelocity) backoff

            -- stop if on ground
            stopIfOnGround moveType trace

          -- check for water transition
          (wasInWater, isInWater) <- checkWaterTransition

          let waterLevel = if isInWater then 1 else 0
          gameBaseGlobals.gbGEdicts.ix edictIdx.eWaterLevel .= waterLevel

          io (putStrLn "SV.physicsToss") >> undefined -- TODO

  where addGravityBasedOnMoveType :: Int -> Quake ()
        addGravityBasedOnMoveType moveType = do
          when (moveType /= Constants.moveTypeFly && moveType /= Constants.moveTypeFlyMissile) $
            addGravity er

        checkGroundEntity :: Quake Bool
        checkGroundEntity = do
          Just velocity <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictPhysics.eVelocity

          when ((velocity^._z) > 0 ) $
            gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictOther.eoGroundEntity .= Nothing

          Just groundEntity <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictOther.eoGroundEntity

          -- check for the groundentity going away
          if isJust groundEntity
            then do
              let Just (EdictReference groundEntityIdx) = groundEntity
              Just groundEntityInUse <- preuse $ gameBaseGlobals.gbGEdicts.ix groundEntityIdx.eInUse
              if not groundEntityInUse
                then do
                  gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictOther.eoGroundEntity .= Nothing
                  return False
                else return True
            else return False
            
        moveAngles :: Quake ()
        moveAngles = do
          Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

          let angles = edict^.eEntityState.esAngles
              avelocity = edict^.eEdictPhysics.eAVelocity
              result = angles + fmap (* Constants.frameTime) avelocity

          gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esAngles .= result

        moveOrigin :: Quake (V3 Float)
        moveOrigin = do
          Just velocity <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictPhysics.eVelocity
          return $ fmap (* Constants.frameTime) velocity

        stopIfOnGround :: Int -> TraceT -> Quake ()
        stopIfOnGround moveType trace = do
          when ((trace^.tPlane.cpNormal._z) > 0.7) $ do
            Just velocity <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictPhysics.eVelocity

            when ((velocity^._z) < 60 || moveType /= Constants.moveTypeBounce) $ do
              let Just (EdictReference traceIdx) = trace^.tEnt
              Just linkCount <- preuse $ gameBaseGlobals.gbGEdicts.ix traceIdx.eLinkCount
              origin <- use $ globals.vec3Origin

              zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
                eEdictOther.eoGroundEntity .= (trace^.tEnt)
                eGroundEntityLinkCount .= linkCount
                eEdictPhysics.eVelocity .= origin
                eEdictPhysics.eAVelocity .= origin

        checkWaterTransition :: Quake (Bool, Bool)
        checkWaterTransition = do
          Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

          let waterType = edict^.eWaterType
              wasInWater = (waterType .&. Constants.maskWater) /= 0
              origin = edict^.eEntityState.esOrigin

          pointContents <- use $ gameBaseGlobals.gbGameImport.giPointContents
          newWaterType <- pointContents origin

          let isInWater = (newWaterType .&. Constants.maskWater) /= 0

          return (wasInWater, isInWater)

push :: EdictReference -> V3 Float -> V3 Float -> Quake Bool
push _ _ _ = io (putStrLn "SV.push") >> undefined -- TODO

{-
- Runs thinking code for this frame if necessary.
-}
runThink :: EdictReference -> Quake Bool
runThink er@(EdictReference edictIdx) = do
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

    let thinktime = edict^.eEdictAction.eaNextThink
    time <- use $ gameBaseGlobals.gbLevel.llTime

    if thinktime <= 0 || thinktime > time + 0.001
      then return True
      else do
        gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictAction.eaNextThink .= 0

        when (isNothing (edict^.eEdictAction.eaThink)) $
          Com.comError Constants.errFatal "NULL ent.think"

        void $ think (fromJust $ edict^.eEdictAction.eaThink) er

        return False

checkVelocity :: EdictReference -> Quake ()
checkVelocity (EdictReference edictIdx) = do
    -- bound velocity
    Just velocity <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictPhysics.eVelocity
    maxVelocityValue <- liftM (^.cvValue) svMaxVelocityCVar

    let boundedVelocity = fmap (boundVelocity maxVelocityValue) velocity

    gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictPhysics.eVelocity .= boundedVelocity

  where boundVelocity :: Float -> Float -> Float
        boundVelocity maxV v = if | v > maxV -> maxV
                                  | v < (-maxV) -> (-maxV)
                                  | otherwise -> v

addGravity :: EdictReference -> Quake ()
addGravity (EdictReference edictIdx) = do
    Just edictGravity <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictPhysics.eGravity
    gravityValue <- liftM (^.cvValue) svGravityCVar

    gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictPhysics.eVelocity._z -= edictGravity * gravityValue * Constants.frameTime

-- Does not change the entities velocity at all
pushEntity :: EdictReference -> V3 Float -> Quake TraceT
pushEntity er@(EdictReference edictIdx) pushV3 = do
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

    let start = edict^.eEntityState.esOrigin
        end = start + pushV3

    -- FIXME: test this
    -- a goto statement was replaced
    traceT <- tryToPush start end

    when (edict^.eInUse) $
      GameBase.touchTriggers er

    return traceT

  where tryToPush :: V3 Float -> V3 Float -> Quake TraceT
        tryToPush start end = do
          Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

          let mask = if edict^.eClipMask /= 0
                       then edict^.eClipMask
                       else Constants.maskSolid

          gameImport <- use $ gameBaseGlobals.gbGameImport

          let trace = gameImport^.giTrace
              linkEntity = gameImport^.giLinkEntity

          traceT <- trace start (edict^.eEdictMinMax.eMins) (edict^.eEdictMinMax.eMaxs) end er mask

          gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esOrigin .= (traceT^.tEndPos)
          linkEntity er

          if traceT^.tFraction /= 1.0
            then do
              impact er traceT

              -- if the pushed entity went away and the pusher is still there
              let Just (EdictReference traceIdx) = traceT^.tEnt
              Just traceEdict <- preuse $ gameBaseGlobals.gbGEdicts.ix traceIdx

              if not(traceEdict^.eInUse) && (edict^.eInUse)
                then do
                  -- move the pusher back and try again
                  gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esOrigin .= start
                  linkEntity er
                  tryToPush start end
                else return traceT
            else return traceT

impact :: EdictReference -> TraceT -> Quake ()
impact _ _ = io (putStrLn "SV.impact") >> undefined -- TODO
