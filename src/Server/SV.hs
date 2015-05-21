{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Server.SV where

import Control.Lens (use, preuse, ix, (^.), (.=), (+=), (-=), (%=), zoom)
import Control.Monad (unless, when, void, liftM)
import Data.Bits ((.&.), (.|.))
import Data.Maybe (isJust, fromJust, isNothing)
import Linear (V3(..), _x, _y, _z, dot)

import Quake
import QuakeState
import CVarVariables
import qualified Constants
import qualified Client.M as M
import qualified Game.GameBase as GameBase
import qualified QCommon.Com as Com
import qualified Server.SVGame as SVGame
import qualified Util.Math3D as Math3D

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

{-
- Monsters freefall when they don't have a ground entity, otherwise all
- movement is done with discrete steps.
- 
- This is also used for objects that have become still on the ground, but
- will fall if the floor is pulled out from under them. FIXME: is this
- true?
-}
physicsStep :: EdictReference -> Quake ()
physicsStep edictRef@(EdictReference edictIdx) = do
    -- airborn monsters should always check for ground
    wasOnGround <- checkGroundEntity

    checkVelocity edictRef

    checkFriction
    
    -- add gravity except:
    --   flying monsters
    --   swiming monsters who are in the water
    hitSound <- checkGravity wasOnGround

    -- friction for flying monsters that have been given vertical velocity
    checkFlyingFriction
    
    -- friction for flying monsters that have been given vertical velocity
    checkSwimmingFriction

    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
    let V3 a b c = edict^.eEdictPhysics.eVelocity

    if a /= 0 || b /= 0 || c /= 0
      then do
        -- apply friction
        -- let dead monsters who aren't completely onground slide
        when (wasOnGround || (edict^.eFlags) .&. (Constants.flSwim .|. Constants.flFly) /= 0) $ do
          ok <- M.checkBottom edictRef
          when (not ((edict^.eEdictStatus.eHealth) <= 0 && not ok)) $ do
            let vel = edict^.eEdictPhysics.eVelocity
                speed = sqrt $ (vel^._x) * (vel^._x) + (vel^._y) * (vel^._y)

            when (speed /= 0) $ do
              let friction = Constants.svFriction
                  control = if speed < Constants.svStopSpeed
                              then Constants.svStopSpeed
                              else speed
                  newSpeed = speed - Constants.frameTime * control * friction
                  newSpeed' = if newSpeed < 0 then 0 else newSpeed / speed

              gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictPhysics.eVelocity._x %= (* newSpeed')
              gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictPhysics.eVelocity._y %= (* newSpeed')

        let mask = if (edict^.eSvFlags) .&. Constants.svfMonster /= 0
                     then Constants.maskMonsterSolid
                     else Constants.maskSolid

        void $ flyMove edictRef Constants.frameTime mask

        gameImport <- use $ gameBaseGlobals.gbGameImport
        let linkEntity = gameImport^.giLinkEntity
            sound = gameImport^.giSound
            soundIndex = gameImport^.giSoundIndex

        linkEntity edictRef
        GameBase.touchTriggers edictRef

        Just edict' <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

        when (edict'^.eInUse) $ do
          when (isJust (edict'^.eEdictOther.eoGroundEntity) && not wasOnGround && hitSound) $ do
            wavIdx <- soundIndex (Just "world/land.wav")
            sound edictRef 0 wavIdx 1 1 0

          void $ runThink edictRef

      else
        void $ runThink edictRef

  where checkGroundEntity :: Quake Bool
        checkGroundEntity = do
          preuse (gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictOther.eoGroundEntity) >>= \(Just groundEntity) ->
            when (isNothing groundEntity) $
              M.checkGround edictRef

          preuse (gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictOther.eoGroundEntity) >>= \(Just groundEntity) ->
            case groundEntity of
              Nothing -> return False
              Just _ -> return True

        checkFriction :: Quake ()
        checkFriction = do
          preuse (gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictPhysics.eAVelocity) >>= \(Just (V3 a b c)) ->
            when (a /= 0 || b /= 0 || c /= 0) $
              addRotationalFriction edictRef

        checkGravity :: Bool -> Quake Bool
        checkGravity wasOnGround = do
          Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

          if not wasOnGround && (edict^.eFlags) .&. Constants.flFly == 0 && not ((edict^.eFlags) .&. Constants.flSwim /= 0 && (edict^.eWaterLevel) > 2)
            then do
              svGravityValue <- liftM (^.cvValue) svGravityCVar
              let hitSound = if (edict^.eEdictPhysics.eVelocity._z) < svGravityValue * (-0.1)
                               then True
                               else False

              when ((edict^.eWaterLevel) == 0) $
                addGravity edictRef

              return hitSound
            else
              return False

        checkFlyingFriction :: Quake ()
        checkFlyingFriction = do
          Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

          when ((edict^.eFlags) .&. Constants.flFly /= 0 && (edict^.eEdictPhysics.eVelocity._z) /= 0) $ do
            let speed = abs (edict^.eEdictPhysics.eVelocity._z)
                control = if speed < Constants.svStopSpeed
                            then Constants.svStopSpeed
                            else speed
                friction = Constants.svFriction / 3
                newSpeed = speed - (Constants.frameTime * control * friction)
                newSpeed' = if newSpeed < 0 then 0 else newSpeed / speed

            gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictPhysics.eVelocity._z %= (* newSpeed')

        checkSwimmingFriction :: Quake ()
        checkSwimmingFriction = do
          Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

          when ((edict^.eFlags) .&. Constants.flSwim /= 0 && (edict^.eEdictPhysics.eVelocity._z) /= 0) $ do
            let speed = abs (edict^.eEdictPhysics.eVelocity._z)
                control = if speed < Constants.svStopSpeed
                            then Constants.svStopSpeed
                            else speed
                newSpeed = speed - (Constants.frameTime * control * Constants.svWaterFriction * (fromIntegral $ edict^.eWaterLevel))
                newSpeed' = if newSpeed < 0 then 0 else newSpeed / speed

            gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictPhysics.eVelocity._z %= (* newSpeed')

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

          playWaterSound oldOrigin wasInWater isInWater

          -- move teamslaves
          Just teamChain <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictOther.eoTeamChain
          Just origin <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esOrigin
          moveTeamSlaves origin teamChain

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

        playWaterSound :: V3 Float -> Bool -> Bool -> Quake ()
        playWaterSound oldOrigin wasInWater isInWater = do
          Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
          gameImport <- use $ gameBaseGlobals.gbGameImport

          let positionedSound = gameImport^.giPositionedSound
              soundIndex = gameImport^.giSoundIndex

          hitwav <- soundIndex (Just "misc/h2ohit1.wav")

          if | not wasInWater && isInWater ->
                 positionedSound oldOrigin er Constants.chanAuto hitwav 1 1 0
             | wasInWater && not isInWater ->
                 positionedSound (edict^.eEntityState.esOrigin) er Constants.chanAuto hitwav 1 1 0
             | otherwise -> return ()

        moveTeamSlaves :: V3 Float -> Maybe EdictReference -> Quake ()
        moveTeamSlaves _ Nothing = return ()
        moveTeamSlaves origin (Just sr@(EdictReference slaveIdx)) = do
          gameBaseGlobals.gbGEdicts.ix slaveIdx.eEntityState.esOrigin .= origin

          linkEntity <- use $ gameBaseGlobals.gbGameImport.giLinkEntity
          linkEntity sr

          Just teamChain <- preuse $ gameBaseGlobals.gbGEdicts.ix slaveIdx.eEdictOther.eoTeamChain
          moveTeamSlaves origin teamChain

{-
- Objects need to be moved back on a failed push, otherwise riders would
- continue to slide.
-}
push :: EdictReference -> V3 Float -> V3 Float -> Quake Bool
push pusherRef@(EdictReference pusherIdx) move amove = do
    -- clamp the move to 1/8 units, so the position will
    -- be accurate for client side prediction
    let updatedMove = fmap clampMove move

    -- find the bounding box
    (mins, maxs) <- findPusherBoundingBox updatedMove

    -- we need this for pushing things later
    vec3origin <- use $ globals.vec3Origin
    let org = vec3origin - amove
        (Just forward, Just right, Just up) = Math3D.angleVectors org True True True

    -- save the pusher's origin position
    savePusherPosition

    -- move the pusher to it's final position
    movePusherToFinalPosition updatedMove
    
    -- see if any solid entities are inside the final position
    numEdicts <- use $ gameBaseGlobals.gbNumEdicts
    done <- checkForSolidEntities (shouldSkip maxs mins) updatedMove (figureMovement forward right up) 1 numEdicts

    if done
      then return False
      else do
        -- FIXME: is there a better way to handle this?
        -- see if anything we moved has touched a trigger
        pushedP <- use $ gameBaseGlobals.gbPushedP
        checkTriggerTouch (pushedP - 1) 0

        return True

  where clampMove :: Float -> Float
        clampMove v =
          let temp = v * 8
              temp' = if temp > 0 then temp + 0.5 else temp - 0.5
              temp'' :: Int = truncate temp'
              temp''' :: Float = fromIntegral temp''
          in temp''' * 0.125

        findPusherBoundingBox :: V3 Float -> Quake (V3 Float, V3 Float)
        findPusherBoundingBox updatedMove = do
          Just eMinMax <- preuse $ gameBaseGlobals.gbGEdicts.ix pusherIdx.eEdictMinMax
          return ((eMinMax^.eAbsMin) + updatedMove, (eMinMax^.eAbsMax) + updatedMove)

        savePusherPosition :: Quake ()
        savePusherPosition = do
          pushedP <- use $ gameBaseGlobals.gbPushedP
          Just pusherEntityState <- preuse $ gameBaseGlobals.gbGEdicts.ix pusherIdx.eEntityState

          zoom (gameBaseGlobals.gbPushed.ix pushedP) $ do
            pEnt .= Just pusherRef
            pOrigin .= (pusherEntityState^.esOrigin)
            pAngles .= (pusherEntityState^.esAngles)

          Just clientRef <- preuse $ gameBaseGlobals.gbGEdicts.ix pusherIdx.eClient

          when (isJust clientRef) $ do
            let Just (GClientReference clientIdx) = clientRef
            Just deltaAngles <- preuse $ gameBaseGlobals.gbGame.glClients.ix clientIdx.gcPlayerState.psPMoveState.pmsDeltaAngles
            gameBaseGlobals.gbPushed.ix pushedP.pDeltaYaw .= fromIntegral (deltaAngles^.(Math3D.v3Access Constants.yaw))

          gameBaseGlobals.gbPushedP += 1

        movePusherToFinalPosition :: V3 Float -> Quake ()
        movePusherToFinalPosition updatedMove = do
          zoom (gameBaseGlobals.gbGEdicts.ix pusherIdx.eEntityState) $ do
            esOrigin %= (+ updatedMove)
            esAngles %= (+ amove)

          linkEntity <- use $ gameBaseGlobals.gbGameImport.giLinkEntity
          linkEntity pusherRef

        checkForSolidEntities :: (EdictReference -> Quake Bool) -> V3 Float -> (EdictReference -> Quake ()) -> Int -> Int -> Quake Bool
        checkForSolidEntities shouldSkip' updatedMove figureMovement' idx maxIdx
          | idx >= maxIdx = return False
          | otherwise = do
              let ref = EdictReference idx
              skip <- shouldSkip' ref

              if not skip
                then do
                  Just pusherMovetype <- preuse $ gameBaseGlobals.gbGEdicts.ix pusherIdx.eMoveType
                  Just edictGroundEntity <- preuse $ gameBaseGlobals.gbGEdicts.ix idx.eEdictOther.eoGroundEntity

                  nextEntity <- if pusherMovetype == Constants.moveTypePush || edictGroundEntity == Just pusherRef
                                  then do
                                    -- move this entity
                                    moveEntity ref

                                    -- try moving the contacted entity
                                    tryMovingContactedEntity updatedMove ref

                                    -- figure movement due to the pusher's amove
                                    figureMovement' ref

                                    -- may have pushed them off an edge
                                    nullifyGroundEntity ref

                                    block <- testEntityPosition ref

                                    if not block
                                      then do -- pushed ok
                                        linkEntity <- use $ gameBaseGlobals.gbGameImport.giLinkEntity
                                        linkEntity ref
                                        -- impact?
                                        return True
                                      else do
                                        -- if it is ok to leave in the old position, do it
                                        -- this is only relevant for riding entities, not pushed
                                        -- FIXME: this doesn't account for rotation
                                        gameBaseGlobals.gbGEdicts.ix idx.eEntityState.esOrigin -= updatedMove
                                        block' <- testEntityPosition ref

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
                      -- save off the obstacle so we can call the block function
                      gameBaseGlobals.gbObstacle .= Just ref

                      -- move back any entities we already moved
                      -- go backwards, so if the same entity was pushed
                      -- twice, it goes back to the original position
                      pushedP <- use $ gameBaseGlobals.gbPushedP
                      moveBackEntity (pushedP - 1) 0

                      return True
                else 
                  checkForSolidEntities shouldSkip' updatedMove figureMovement' (idx + 1) maxIdx

        moveBackEntity :: Int -> Int -> Quake ()
        moveBackEntity idx minIdx
          | idx >= minIdx = do
              Just p <- preuse $ gameBaseGlobals.gbPushed.ix idx
              let Just edictRef@(EdictReference edictIdx) = p^.pEnt

              zoom (gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState) $ do
                esOrigin .= (p^.pOrigin)
                esAngles .= (p^.pAngles)

              Just clientRef <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eClient
              when (isJust clientRef) $ do
                let Just (GClientReference clientIdx) = clientRef
                    -- ugly :( i know i know
                    access = if | Constants.yaw == 0 -> _x
                                | Constants.yaw == 1 -> _y
                                | otherwise -> _z
                gameBaseGlobals.gbGame.glClients.ix clientIdx.gcPlayerState.psPMoveState.pmsDeltaAngles.(access) .= truncate (p^.pDeltaYaw)

              linkEntity <- use $ gameBaseGlobals.gbGameImport.giLinkEntity
              linkEntity edictRef
          | otherwise = return ()

        nullifyGroundEntity :: EdictReference -> Quake ()
        nullifyGroundEntity (EdictReference edictIdx) = do
          Just groundEntity <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictOther.eoGroundEntity
          when (groundEntity /= Just pusherRef) $
            gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictOther.eoGroundEntity .= Nothing

        figureMovement :: V3 Float -> V3 Float -> V3 Float -> EdictReference -> Quake ()
        figureMovement forward right up (EdictReference edictIdx) = do
          Just edictOrigin <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esOrigin
          Just pusherOrigin <- preuse $ gameBaseGlobals.gbGEdicts.ix pusherIdx.eEntityState.esOrigin
          
          let org = edictOrigin - pusherOrigin
              org2 = V3 (dot org forward) (dot org right) (dot org up)
              move2 = org2 - org

          gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esOrigin += move2

        tryMovingContactedEntity :: V3 Float -> EdictReference -> Quake ()
        tryMovingContactedEntity updatedMove (EdictReference edictIdx) = do
          gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esOrigin += updatedMove
          Just clientRef <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eClient

          when (isJust clientRef) $ do -- FIXME: doesn't rotate monsters?
            let Just (GClientReference clientIdx) = clientRef
                -- ugly :( i know i know
                access = if | Constants.yaw == 0 -> _x
                            | Constants.yaw == 1 -> _y
                            | otherwise -> _z
            gameBaseGlobals.gbGame.glClients.ix clientIdx.gcPlayerState.psPMoveState.pmsDeltaAngles.(access) += (truncate $ amove^.(Math3D.v3Access Constants.yaw))

        moveEntity :: EdictReference -> Quake ()
        moveEntity edictRef@(EdictReference edictIdx) = do
          pushedP <- use $ gameBaseGlobals.gbPushedP
          Just entityState <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState

          zoom (gameBaseGlobals.gbPushed.ix pushedP) $ do
            pEnt .= Just edictRef
            pOrigin .= (entityState^.esOrigin)
            pAngles .= (entityState^.esAngles)

          gameBaseGlobals.gbPushedP += 1

        shouldSkip :: V3 Float -> V3 Float -> EdictReference -> Quake Bool
        shouldSkip maxs mins (EdictReference edictIdx) = do
          Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
          linked <- isLinkedAnywhere (edict^.eArea)

          if | not (edict^.eInUse) -> return True
             | any (== (edict^.eMoveType)) [Constants.moveTypePush, Constants.moveTypeStop, Constants.moveTypeNone, Constants.moveTypeNoClip] -> return True
               -- not linked in anywhere
             | not linked -> return True
               -- if the entity is standing on the pusher, it will definetly be moved
             | (edict^.eEdictOther.eoGroundEntity) /= Just (EdictReference edictIdx) -> do
                 -- see if the ent needs to be tested
                 let absmin = edict^.eEdictMinMax.eAbsMin
                     absmax = edict^.eEdictMinMax.eAbsMax

                 if absmin^._x >= maxs^._x || absmin^._y >= maxs^._y || absmin^._z >= maxs^._z ||
                    absmax^._x <= mins^._x || absmax^._y <= mins^._y || absmax^._z <= mins^._z
                    then return True
                    else
                      -- see if the ent's bbox is inside the pusher's final position
                      liftM not (testEntityPosition (EdictReference edictIdx))
             | otherwise -> return False
        
        isLinkedAnywhere :: LinkReference -> Quake Bool
        isLinkedAnywhere (LinkReference linkIdx) = do
          Just link <- preuse $ svGlobals.svLinks.ix linkIdx
          return $ isJust (link^.lPrev)

        checkTriggerTouch :: Int -> Int -> Quake ()
        checkTriggerTouch idx minIdx
          | idx >= minIdx = do
              Just edictRef <- preuse $ gameBaseGlobals.gbPushed.ix idx.pEnt
              GameBase.touchTriggers (fromJust edictRef)
          | otherwise = return ()

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

          traceT <- trace start (Just $ edict^.eEdictMinMax.eMins) (Just $ edict^.eEdictMinMax.eMaxs) end er mask

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

-- Two entites have touched, so run their touch functions
impact :: EdictReference -> TraceT -> Quake ()
impact er@(EdictReference edictIdx) traceT = do
    let Just tr@(EdictReference traceIdx) = traceT^.tEnt

    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
    Just traceEdict <- preuse $ gameBaseGlobals.gbGEdicts.ix traceIdx

    when (isJust (edict^.eEdictAction.eaTouch) && (edict^.eSolid) /= Constants.solidNot) $
      touch (fromJust $ edict^.eEdictAction.eaTouch) er tr (traceT^.tPlane) (traceT^.tSurface)

    dummyPlane <- use $ gameBaseGlobals.gbDummyPlane

    when (isJust (traceEdict^.eEdictAction.eaTouch) && (traceEdict^.eSolid) /= Constants.solidNot) $
      touch (fromJust $ traceEdict^.eEdictAction.eaTouch) tr er dummyPlane Nothing

testEntityPosition :: EdictReference -> Quake Bool
testEntityPosition (EdictReference edictIdx) = do
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

    let mask = if (edict^.eClipMask) /= 0
                 then edict^.eClipMask
                 else Constants.maskSolid

    trace <- use $ gameBaseGlobals.gbGameImport.giTrace
    traceT <- trace (edict^.eEntityState.esOrigin) (Just $ edict^.eEdictMinMax.eMins) (Just $ edict^.eEdictMinMax.eMaxs) (edict^.eEntityState.esOrigin) (EdictReference edictIdx) mask

    return (traceT^.tStartSolid)

-- FIXME: hacked in for E3 demo
addRotationalFriction :: EdictReference -> Quake ()
addRotationalFriction _ = do
    io (putStrLn "SV.addRotationalFriction") >> undefined -- TODO

flyMove :: EdictReference -> Float -> Int -> Quake Int
flyMove _ _ _ = do
    io (putStrLn "SV.flyMove") >> undefined -- TODO
