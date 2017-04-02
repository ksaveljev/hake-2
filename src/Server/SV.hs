{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Server.SV where

import Control.Lens (use, preuse, ix, (^.), (.=), (+=), (-=), (%=), zoom, (%~), (&), (.~), (%~), (+~), (-~))
import Control.Monad (unless, when, void, liftM)
import Data.Bits ((.&.), (.|.), complement)
import Data.Maybe (isJust, fromJust, isNothing)
import Linear (V3(..), _x, _y, _z, dot, cross)
import qualified Data.Vector as V
import qualified Data.ByteString as B

import Game.GClientT
import Game.MoveInfoT
import Game.ClientPersistantT
import Game.ClientRespawnT
import Game.MonsterInfoT
import Game.PlayerStateT
import Types
import Game.PMoveStateT
import QuakeRef
import QuakeState
import CVarVariables
import qualified Constants
import qualified Client.M as M
import {-# SOURCE #-} qualified Game.GameBase as GameBase
import qualified QCommon.Com as Com
import qualified Server.SVGame as SVGame
import qualified Util.Lib as Lib
import qualified Util.Math3D as Math3D

import Game.Adapters

diNoDir :: Float
diNoDir = -1

maxClipPlanes :: Int
maxClipPlanes = 5

{-
- 
- Bmodel objects don't interact with each other, but push all box objects.
-}
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
      finalEdict <- pushTeamChain (Just edictRef)

      pushedP <- use $ gameBaseGlobals.gbPushedP

      when (pushedP > Constants.maxEdicts) $
        SVGame.pfError2 Constants.errFatal "pushed_p > &pushed[MAX_EDICTS], memory corrupted"

      case finalEdict of
        Just blockedRef -> do
          -- the move failed, bump all nextthink times and back out moves
          backOutTeamChain (Just edictRef)
          blockedEdict <- readRef blockedRef

          when (isJust (blockedEdict^.eBlocked)) $ do
            obstacle <- use $ gameBaseGlobals.gbObstacle
            blocked (fromJust $ blockedEdict^.eBlocked) blockedRef (fromJust obstacle)

        Nothing ->
          -- the move succeeded, so call all think functions
          thinkTeamChain (Just edictRef)

  where pushTeamChain :: Maybe (Ref EdictT) -> Quake (Maybe (Ref EdictT))
        pushTeamChain Nothing = return Nothing
        pushTeamChain (Just chainRef) = do
          edict <- readRef chainRef
          
          let velocity = edict^.eVelocity
              avelocity = edict^.eAVelocity

          if (velocity^._x) /= 0 || (velocity^._y) /= 0 || (velocity^._z) /= 0 || (avelocity^._x) /= 0 || (avelocity^._y) /= 0 || (avelocity^._z) /= 0
            then do
              -- object is moving
              let move = fmap (* Constants.frameTime) velocity
                  amove = fmap (* Constants.frameTime) avelocity

              pushed <- push chainRef move amove

              if pushed
                then pushTeamChain (edict^.eTeamChain)
                else return (Just chainRef)

            else pushTeamChain (edict^.eTeamChain)

        thinkTeamChain :: Maybe (Ref EdictT) -> Quake ()
        thinkTeamChain Nothing = return ()
        thinkTeamChain (Just chainRef) = do
          edict <- readRef chainRef

          void $ runThink chainRef

          thinkTeamChain (edict^.eTeamChain)

        backOutTeamChain :: Maybe (Ref EdictT) -> Quake ()
        backOutTeamChain Nothing = return ()
        backOutTeamChain (Just chainRef) = do
          edict <- readRef chainRef

          when ((edict^.eNextThink) > 0) $
            modifyRef chainRef (\v -> v & eNextThink +~ Constants.frameTime)

          backOutTeamChain (edict^.eTeamChain)

-- Non moving objects can only think
physicsNone :: Ref EdictT -> Quake ()
physicsNone = void . runThink -- regular thinking

physicsNoClip :: Ref EdictT -> Quake ()
physicsNoClip _ = io (putStrLn "SV.physicsNoClip") >> undefined -- TODO

{-
- Monsters freefall when they don't have a ground entity, otherwise all
- movement is done with discrete steps.
- 
- This is also used for objects that have become still on the ground, but
- will fall if the floor is pulled out from under them. FIXME: is this
- true?
-}
physicsStep :: Ref EdictT -> Quake ()
physicsStep edictRef = do
    -- io (print "PHYSICS STEP")
    -- io (print edictIdx)

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

    edict <- readRef edictRef
    let V3 a b c = edict^.eVelocity

    -- io (print $ "VELOCITY = " ++ show a ++ " " ++ show b ++ " " ++ show c)

    if a /= 0 || b /= 0 || c /= 0
      then do
        -- apply friction
        -- let dead monsters who aren't completely onground slide
        when (wasOnGround || (edict^.eFlags) .&. (Constants.flSwim .|. Constants.flFly) /= 0) $ do
          ok <- M.checkBottom edictRef

          when (not ((edict^.eHealth) <= 0 && not ok)) $ do
            let vel = edict^.eVelocity
                speed = sqrt $ (vel^._x) * (vel^._x) + (vel^._y) * (vel^._y)

            when (speed /= 0) $ do
              let friction = Constants.svFriction
                  control = if speed < Constants.svStopSpeed
                              then Constants.svStopSpeed
                              else speed
                  newSpeed = speed - Constants.frameTime * control * friction
                  newSpeed' = if newSpeed < 0 then 0 else newSpeed / speed

              modifyRef edictRef (\v -> v & eVelocity._x %~ (* newSpeed')
                                            & eVelocity._y %~ (* newSpeed'))

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

        edict' <- readRef edictRef

        when (edict'^.eInUse) $ do
          when (isJust (edict'^.eGroundEntity) && not wasOnGround && hitSound) $ do
            wavIdx <- soundIndex (Just "world/land.wav")
            sound (Just edictRef) 0 wavIdx 1 1 0

          void $ runThink edictRef

      else
        void $ runThink edictRef

  where checkGroundEntity :: Quake Bool
        checkGroundEntity = do
          readRef edictRef >>= \edict ->
            when (isNothing (edict^.eGroundEntity)) $
              M.checkGround edictRef

          readRef edictRef >>= \edict ->
            case edict^.eGroundEntity of
              Nothing -> return False
              Just _ -> return True

        checkFriction :: Quake ()
        checkFriction = do
          readRef edictRef >>= \edict -> do
            let V3 a b c = edict^.eAVelocity
            when (a /= 0 || b /= 0 || c /= 0) $
              addRotationalFriction edictRef

        checkGravity :: Bool -> Quake Bool
        checkGravity wasOnGround = do
          edict <- readRef edictRef

          if not wasOnGround && (edict^.eFlags) .&. Constants.flFly == 0 && not ((edict^.eFlags) .&. Constants.flSwim /= 0 && (edict^.eWaterLevel) > 2)
            then do
              svGravityValue <- liftM (^.cvValue) svGravityCVar
              let hitSound = if (edict^.eVelocity._z) < svGravityValue * (-0.1)
                               then True
                               else False

              when ((edict^.eWaterLevel) == 0) $
                addGravity edictRef

              return hitSound
            else
              return False

        checkFlyingFriction :: Quake ()
        checkFlyingFriction = do
          edict <- readRef edictRef

          when ((edict^.eFlags) .&. Constants.flFly /= 0 && (edict^.eVelocity._z) /= 0) $ do
            let speed = abs (edict^.eVelocity._z)
                control = if speed < Constants.svStopSpeed
                            then Constants.svStopSpeed
                            else speed
                friction = Constants.svFriction / 3
                newSpeed = speed - (Constants.frameTime * control * friction)
                newSpeed' = if newSpeed < 0 then 0 else newSpeed / speed

            modifyRef edictRef (\v -> v & eVelocity._z %~ (* newSpeed'))

        checkSwimmingFriction :: Quake ()
        checkSwimmingFriction = do
          edict <- readRef edictRef

          when ((edict^.eFlags) .&. Constants.flSwim /= 0 && (edict^.eVelocity._z) /= 0) $ do
            let speed = abs (edict^.eVelocity._z)
                control = if speed < Constants.svStopSpeed
                            then Constants.svStopSpeed
                            else speed
                newSpeed = speed - (Constants.frameTime * control * Constants.svWaterFriction * (fromIntegral $ edict^.eWaterLevel))
                newSpeed' = if newSpeed < 0 then 0 else newSpeed / speed

            modifyRef edictRef (\v -> v & eVelocity._z %~ (* newSpeed'))

-- Toss, bounce, and fly movement. When onground, do nothing
physicsToss :: Ref EdictT -> Quake ()
physicsToss edictRef = do
    -- regular thinking
    void $ runThink edictRef

    edictFlags <- readRef edictRef >>= \edict -> return (edict^.eFlags)

    -- if not a team captain, so movement will be handled elsewhere
    unless (edictFlags .&. Constants.flTeamSlave /= 0) $ do
      onGround <- checkGroundEntity

      -- if onground, return without moving
      unless onGround $ do
        oldOrigin <- readRef edictRef >>= \edict -> return (edict^.eEntityState.esOrigin)

        checkVelocity edictRef

        -- add gravity
        moveType <- readRef edictRef >>= \edict -> return (edict^.eMoveType)
        addGravityBasedOnMoveType moveType

        -- move angles
        moveAngles

        -- move origin
        move <- moveOrigin
        trace <- pushEntity edictRef move

        inUse <- readRef edictRef >>= \edict -> return (edict^.eInUse)

        when inUse $ do
          when (trace^.tFraction < 1) $ do
            let backoff = if moveType == Constants.moveTypeBounce
                            then 1.5
                            else 1

            velocity <- readRef edictRef >>= \edict -> return (edict^.eVelocity)
            let (_, out) = GameBase.clipVelocity velocity (trace^.tPlane.cpNormal) backoff
            modifyRef edictRef (\v -> v & eVelocity .~ out)

            -- stop if on ground
            stopIfOnGround moveType trace

          -- check for water transition
          (wasInWater, isInWater) <- checkWaterTransition

          let waterLevel = if isInWater then 1 else 0
          modifyRef edictRef (\v -> v & eWaterLevel .~ waterLevel)

          playWaterSound oldOrigin wasInWater isInWater

          -- move teamslaves
          teamChain <- readRef edictRef >>= \edict -> return (edict^.eTeamChain)
          origin <- readRef edictRef >>= \edict -> return (edict^.eEntityState.esOrigin)
          moveTeamSlaves origin teamChain

  where addGravityBasedOnMoveType :: Int -> Quake ()
        addGravityBasedOnMoveType moveType = do
          when (moveType /= Constants.moveTypeFly && moveType /= Constants.moveTypeFlyMissile) $
            addGravity edictRef

        checkGroundEntity :: Quake Bool
        checkGroundEntity = do
          velocity <- readRef edictRef >>= \edict -> return (edict^.eVelocity)

          when ((velocity^._z) > 0 ) $
            modifyRef edictRef (\v -> v & eGroundEntity .~ Nothing)

          groundEntity <- readRef edictRef >>= \edict -> return (edict^.eGroundEntity)

          -- check for the groundentity going away
          case groundEntity of
            Just groundEntityRef -> do
              groundEntityInUse <- readRef groundEntityRef >>= \e -> return (e^.eInUse)
              if not groundEntityInUse
                then do
                  modifyRef edictRef (\v -> v & eGroundEntity .~ Nothing)
                  return False
                else
                  return True

            Nothing ->
              return False
            
        moveAngles :: Quake ()
        moveAngles = do
          edict <- readRef edictRef

          let angles = edict^.eEntityState.esAngles
              avelocity = edict^.eAVelocity
              result = angles + fmap (* Constants.frameTime) avelocity

          modifyRef edictRef (\v -> v & eEntityState.esAngles .~ result)

        moveOrigin :: Quake (V3 Float)
        moveOrigin = do
          edict <- readRef edictRef
          return $ fmap (* Constants.frameTime) (edict^.eVelocity)

        stopIfOnGround :: Int -> TraceT -> Quake ()
        stopIfOnGround moveType trace = do
          when ((trace^.tPlane.cpNormal._z) > 0.7) $ do
            velocity <- readRef edictRef >>= \e -> return (e^.eVelocity)

            when ((velocity^._z) < 60 || moveType /= Constants.moveTypeBounce) $ do
              let Just traceRef = trace^.tEnt
              linkCount <- readRef traceRef >>= \e -> return (e^.eLinkCount)
              origin <- use $ globals.gVec3Origin

              modifyRef edictRef (\v -> v & eGroundEntity .~ (trace^.tEnt)
                                             & eGroundEntityLinkCount .~ linkCount
                                             & eVelocity .~ origin
                                             & eAVelocity .~ origin)

        checkWaterTransition :: Quake (Bool, Bool)
        checkWaterTransition = do
          edict <- readRef edictRef

          let waterType = edict^.eWaterType
              wasInWater = (waterType .&. Constants.maskWater) /= 0
              origin = edict^.eEntityState.esOrigin

          pointContents <- use $ gameBaseGlobals.gbGameImport.giPointContents
          newWaterType <- pointContents origin

          let isInWater = (newWaterType .&. Constants.maskWater) /= 0

          return (wasInWater, isInWater)

        playWaterSound :: V3 Float -> Bool -> Bool -> Quake ()
        playWaterSound oldOrigin wasInWater isInWater = do
          edict <- readRef edictRef
          gameImport <- use $ gameBaseGlobals.gbGameImport

          let positionedSound = gameImport^.giPositionedSound
              soundIndex = gameImport^.giSoundIndex

          hitwav <- soundIndex (Just "misc/h2ohit1.wav")

          if | not wasInWater && isInWater ->
                 positionedSound (Just oldOrigin) edictRef Constants.chanAuto hitwav 1 1 0
             | wasInWater && not isInWater ->
                 positionedSound (Just $ edict^.eEntityState.esOrigin) edictRef Constants.chanAuto hitwav 1 1 0
             | otherwise -> return ()

        moveTeamSlaves :: V3 Float -> Maybe (Ref EdictT) -> Quake ()
        moveTeamSlaves _ Nothing = return ()
        moveTeamSlaves origin (Just slaveRef) = do
          modifyRef slaveRef (\v -> v & eEntityState.esOrigin .~ origin)

          linkEntity <- use $ gameBaseGlobals.gbGameImport.giLinkEntity
          linkEntity slaveRef

          teamChain <- readRef slaveRef >>= \e -> return (e^.eTeamChain)
          moveTeamSlaves origin teamChain

{-
- Objects need to be moved back on a failed push, otherwise riders would
- continue to slide.
-}
push :: Ref EdictT -> V3 Float -> V3 Float -> Quake Bool
push pusherRef move amove = do
    -- clamp the move to 1/8 units, so the position will
    -- be accurate for client side prediction
    let updatedMove = fmap clampMove move

    -- find the bounding box
    (mins, maxs) <- findPusherBoundingBox updatedMove

    -- we need this for pushing things later
    vec3origin <- use $ globals.gVec3Origin
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
      then
        return False
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
          pusher <- readRef pusherRef
          return ((pusher^.eAbsMin) + updatedMove, (pusher^.eAbsMax) + updatedMove)

        savePusherPosition :: Quake ()
        savePusherPosition = do
          pushedP <- use $ gameBaseGlobals.gbPushedP
          pusher <- readRef pusherRef

          let pusherEntityState = pusher^.eEntityState

          zoom (gameBaseGlobals.gbPushed.ix pushedP) $ do
            pEnt .= Just pusherRef
            pOrigin .= (pusherEntityState^.esOrigin)
            pAngles .= (pusherEntityState^.esAngles)

          let clientRef = pusher^.eClient

          when (isJust clientRef) $ do
            let Just (Ref clientIdx) = clientRef
            Just deltaAngles <- preuse $ gameBaseGlobals.gbGame.glClients.ix clientIdx.gcPlayerState.psPMoveState.pmsDeltaAngles
            gameBaseGlobals.gbPushed.ix pushedP.pDeltaYaw .= fromIntegral (deltaAngles^.(Math3D.v3Access Constants.yaw))

          gameBaseGlobals.gbPushedP += 1

        movePusherToFinalPosition :: V3 Float -> Quake ()
        movePusherToFinalPosition updatedMove = do
          modifyRef pusherRef (\v -> v & eEntityState.esOrigin %~ (+ updatedMove)
                                          & eEntityState.esAngles %~ (+ amove))

          linkEntity <- use $ gameBaseGlobals.gbGameImport.giLinkEntity
          linkEntity pusherRef

        checkForSolidEntities :: (Ref EdictT -> Quake Bool) -> V3 Float -> (Ref EdictT -> Quake ()) -> Int -> Int -> Quake Bool
        checkForSolidEntities shouldSkip' updatedMove figureMovement' idx maxIdx
          | idx >= maxIdx = return False
          | otherwise = do
              let ref = Ref idx
              skip <- shouldSkip' ref

              if not skip
                then do
                  pusherMoveType <- readRef pusherRef >>= \e -> return (e^.eMoveType)
                  edictGroundEntity <- readRef ref >>= \e -> return (e^.eGroundEntity)

                  nextEntity <- if pusherMoveType == Constants.moveTypePush || edictGroundEntity == Just pusherRef
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
                                        modifyRef ref (\v -> v & eEntityState.esOrigin -~ updatedMove)
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
              let Just edictRef = p^.pEnt

              modifyRef edictRef (\v -> v & eEntityState.esOrigin .~ (p^.pOrigin)
                                             & eEntityState.esAngles .~ (p^.pAngles))

              clientRef <- readRef edictRef >>= \e -> return (e^.eClient)

              when (isJust clientRef) $ do
                let Just (Ref clientIdx) = clientRef
                    -- ugly :( i know i know
                    access = if | Constants.yaw == 0 -> _x
                                | Constants.yaw == 1 -> _y
                                | otherwise -> _z

                gameBaseGlobals.gbGame.glClients.ix clientIdx.gcPlayerState.psPMoveState.pmsDeltaAngles.(access) .= truncate (p^.pDeltaYaw)

              linkEntity <- use $ gameBaseGlobals.gbGameImport.giLinkEntity
              linkEntity edictRef

          | otherwise = return ()

        nullifyGroundEntity :: Ref EdictT -> Quake ()
        nullifyGroundEntity edictRef = do
          edict <- readRef edictRef
          let groundEntity = edict^.eGroundEntity
          when (groundEntity /= Just pusherRef) $
            modifyRef edictRef (\v -> v & eGroundEntity .~ Nothing)

        figureMovement :: V3 Float -> V3 Float -> V3 Float -> Ref EdictT -> Quake ()
        figureMovement forward right up edictRef = do
          edictOrigin <- readRef edictRef >>= \e -> return (e^.eEntityState.esOrigin)
          pusherOrigin <- readRef pusherRef >>= \e -> return (e^.eEntityState.esOrigin)
          
          let org = edictOrigin - pusherOrigin
              org2 = V3 (dot org forward) (dot org right) (dot org up)
              move2 = org2 - org

          modifyRef edictRef (\v -> v & eEntityState.esOrigin +~ move2)

        tryMovingContactedEntity :: V3 Float -> Ref EdictT -> Quake ()
        tryMovingContactedEntity updatedMove edictRef = do
          modifyRef edictRef (\v -> v & eEntityState.esOrigin +~ updatedMove)
          clientRef <- readRef edictRef >>= \e -> return (e^.eClient)

          when (isJust clientRef) $ do -- FIXME: doesn't rotate monsters?
            let Just (Ref clientIdx) = clientRef
                -- ugly :( i know i know
                access = if | Constants.yaw == 0 -> _x
                            | Constants.yaw == 1 -> _y
                            | otherwise -> _z

            gameBaseGlobals.gbGame.glClients.ix clientIdx.gcPlayerState.psPMoveState.pmsDeltaAngles.(access) += (truncate $ amove^.(Math3D.v3Access Constants.yaw))

        moveEntity :: Ref EdictT -> Quake ()
        moveEntity edictRef = do
          pushedP <- use $ gameBaseGlobals.gbPushedP
          entityState <- readRef edictRef >>= \e -> return (e^.eEntityState)

          zoom (gameBaseGlobals.gbPushed.ix pushedP) $ do
            pEnt .= Just edictRef
            pOrigin .= (entityState^.esOrigin)
            pAngles .= (entityState^.esAngles)

          gameBaseGlobals.gbPushedP += 1

        shouldSkip :: V3 Float -> V3 Float -> Ref EdictT -> Quake Bool
        shouldSkip maxs mins edictRef = do
          edict <- readRef edictRef
          linked <- isLinkedAnywhere (edict^.eArea)

          if | not (edict^.eInUse) -> return True

             | any (== (edict^.eMoveType)) [Constants.moveTypePush, Constants.moveTypeStop, Constants.moveTypeNone, Constants.moveTypeNoClip] -> return True

               -- not linked in anywhere
             | not linked -> return True

               -- if the entity is standing on the pusher, it will definetly be moved
             | (edict^.eGroundEntity) /= Just edictRef -> do
                 -- see if the ent needs to be tested
                 let absmin = edict^.eAbsMin
                     absmax = edict^.eAbsMax

                 if absmin^._x >= maxs^._x || absmin^._y >= maxs^._y || absmin^._z >= maxs^._z ||
                    absmax^._x <= mins^._x || absmax^._y <= mins^._y || absmax^._z <= mins^._z
                    then return True
                    else
                      -- see if the ent's bbox is inside the pusher's final position
                      liftM not (testEntityPosition edictRef)

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
runThink :: Ref EdictT -> Quake Bool
runThink edictRef = do
    edict <- readRef edictRef

    let thinkTime = edict^.eNextThink
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    if thinkTime <= 0 || thinkTime > levelTime + 0.001
      then
        return True

      else do
        modifyRef edictRef (\v -> v & eNextThink .~ 0)

        when (isNothing (edict^.eThink)) $
          Com.comError Constants.errFatal "NULL ent.think"

        let GenericEntThink sdf _ = fromJust $ edict^.eThink
        -- io (print $ "THINK FUNCTION = " `B.append` sdf)
        void $ think (fromJust $ edict^.eThink) edictRef

        return False

checkVelocity :: Ref EdictT -> Quake ()
checkVelocity edictRef = do
    -- bound velocity
    velocity <- readRef edictRef >>= \e -> return (e^.eVelocity)
    maxVelocityValue <- liftM (^.cvValue) svMaxVelocityCVar

    let boundedVelocity = fmap (boundVelocity maxVelocityValue) velocity

    modifyRef edictRef (\v -> v & eVelocity .~ boundedVelocity)

  where boundVelocity :: Float -> Float -> Float
        boundVelocity maxV v = if | v > maxV -> maxV
                                  | v < (-maxV) -> (-maxV)
                                  | otherwise -> v

addGravity :: Ref EdictT -> Quake ()
addGravity edictRef = do
    edictGravity <- readRef edictRef >>= \e -> return (e^.eGravity)
    gravityValue <- liftM (^.cvValue) svGravityCVar

    modifyRef edictRef (\v -> v & eVelocity._z -~ edictGravity * gravityValue * Constants.frameTime)

-- Does not change the entities velocity at all
pushEntity :: Ref EdictT -> V3 Float -> Quake TraceT
pushEntity edictRef pushV3 = do
    edict <- readRef edictRef

    let start = edict^.eEntityState.esOrigin
        end = start + pushV3

    -- FIXME: test this
    -- a goto statement was replaced
    traceT <- tryToPush start end

    when (edict^.eInUse) $
      GameBase.touchTriggers edictRef

    return traceT

  where tryToPush :: V3 Float -> V3 Float -> Quake TraceT
        tryToPush start end = do
          edict <- readRef edictRef

          let mask = if edict^.eClipMask /= 0
                       then edict^.eClipMask
                       else Constants.maskSolid

          gameImport <- use $ gameBaseGlobals.gbGameImport

          let trace = gameImport^.giTrace
              linkEntity = gameImport^.giLinkEntity

          traceT <- trace start
                          (Just $ edict^.eMins)
                          (Just $ edict^.eMaxs)
                          end
                          (Just edictRef)
                          mask

          modifyRef edictRef (\v -> v & eEntityState.esOrigin .~ (traceT^.tEndPos))
          linkEntity edictRef

          if traceT^.tFraction /= 1.0
            then do
              impact edictRef traceT

              -- if the pushed entity went away and the pusher is still there
              let Just traceRef = traceT^.tEnt
              traceEdict <- readRef traceRef

              if not(traceEdict^.eInUse) && (edict^.eInUse)
                then do
                  -- move the pusher back and try again
                  modifyRef edictRef (\v -> v & eEntityState.esOrigin .~ start)
                  linkEntity edictRef
                  tryToPush start end

                else
                  return traceT

            else
              return traceT

-- Two entites have touched, so run their touch functions
impact :: Ref EdictT -> TraceT -> Quake ()
impact edictRef traceT = do
    let Just traceRef = traceT^.tEnt

    edict <- readRef edictRef
    traceEdict <- readRef traceRef

    when (isJust (edict^.eTouch) && (edict^.eSolid) /= Constants.solidNot) $
      touch (fromJust $ edict^.eTouch) edictRef traceRef (traceT^.tPlane) (traceT^.tSurface)

    dummyPlane <- use $ gameBaseGlobals.gbDummyPlane

    when (isJust (traceEdict^.eTouch) && (traceEdict^.eSolid) /= Constants.solidNot) $
      touch (fromJust $ traceEdict^.eTouch) traceRef edictRef dummyPlane Nothing

testEntityPosition :: Ref EdictT -> Quake Bool
testEntityPosition edictRef = do
    edict <- readRef edictRef

    let mask = if (edict^.eClipMask) /= 0
                 then edict^.eClipMask
                 else Constants.maskSolid

    trace <- use $ gameBaseGlobals.gbGameImport.giTrace
    traceT <- trace (edict^.eEntityState.esOrigin)
                    (Just $ edict^.eMins)
                    (Just $ edict^.eMaxs)
                    (edict^.eEntityState.esOrigin)
                    (Just edictRef)
                    mask

    return (traceT^.tStartSolid)

-- FIXME: hacked in for E3 demo
addRotationalFriction :: Ref EdictT -> Quake ()
addRotationalFriction _ = do
    io (putStrLn "SV.addRotationalFriction") >> undefined -- TODO

{-
- SV_FlyMove
- 
- The basic solid body movement clip that slides along multiple planes
- Returns the clipflags if the velocity was modified (hit something solid)
- 1 = floor 2 = wall / step 4 = dead stop
-}
flyMove :: Ref EdictT -> Float -> Int -> Quake Int
flyMove edictRef time mask = do
    edict <- readRef edictRef
    let velocity = edict^.eVelocity

    modifyRef edictRef (\v -> v & eGroundEntity .~ Nothing)
    let planes = V.replicate 6 (V3 0 0 0)

    v <- doFlyMove velocity velocity planes time 0 0 0 4
    -- io (putStrLn "HIHIHI")
    -- io (print v)
    return v

  where doFlyMove :: V3 Float -> V3 Float -> V.Vector (V3 Float) -> Float -> Int -> Int -> Int -> Int -> Quake Int
        doFlyMove primalVelocity originalVelocity planes timeLeft numPlanes blockedMask idx maxIdx
          | idx >= maxIdx = return blockedMask
          | otherwise = do
              traceT <- readRef edictRef >>= \edict -> do
                          let end = (edict^.eEntityState.esOrigin) + fmap (* timeLeft) (edict^.eVelocity)
                          trace <- use $ gameBaseGlobals.gbGameImport.giTrace
                          trace (edict^.eEntityState.esOrigin)
                                (Just $ edict^.eMins)
                                (Just $ edict^.eMaxs)
                                end
                                (Just edictRef)
                                mask

              if (traceT^.tAllSolid) -- entity is trapped in another solid
                then do
                  use (globals.gVec3Origin) >>= \v3o ->
                    modifyRef edictRef (\v -> v & eVelocity .~ v3o)
                  return 3
                else do
                  (numPlanes', originalVelocity') <- if (traceT^.tFraction) > 0 -- actually covered some distance
                                                       then do
                                                         modifyRef edictRef (\v -> v & eEntityState.esOrigin .~ traceT^.tEndPos)
                                                         v <- readRef edictRef >>= \e -> return (e^.eVelocity)
                                                         return (0, v)
                                                       else
                                                         return (numPlanes, originalVelocity)

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
                          if numPlanes' >= maxClipPlanes -- this shouldn't really happen
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
                                      doFlyMove primalVelocity originalVelocity' planes' timeLeft' numPlanes'' blockedMask' (idx + 1) maxIdx

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
                                          doFlyMove primalVelocity originalVelocity' planes' timeLeft' numPlanes'' blockedMask' (idx + 1) maxIdx

        modifyOriginVelocity :: V.Vector (V3 Float) -> V3 Float -> Int -> Int -> V3 Float -> (Int, V3 Float)
        modifyOriginVelocity planes originalVelocity idx maxIdx newVelocity
          | idx >= maxIdx = (idx, newVelocity)
          | otherwise =
              let (_, newVelocity') = GameBase.clipVelocity originalVelocity (planes V.! idx) 1
                  j = checkPlanes planes newVelocity' idx 0 maxIdx
              in if j == maxIdx
                   then (idx, newVelocity')
                   else modifyOriginVelocity planes originalVelocity (idx + 1) maxIdx newVelocity'

        checkPlanes :: V.Vector (V3 Float) -> V3 Float -> Int -> Int -> Int -> Int
        checkPlanes planes newVelocity i idx maxIdx
          | idx >= maxIdx = idx
          | otherwise =
              if idx /= i && (planes V.! idx) /= (planes V.! i)
                then if newVelocity `dot` (planes V.! idx) < 0 -- not ok
                       then idx
                       else checkPlanes planes newVelocity i (idx + 1) maxIdx
                else checkPlanes planes newVelocity i (idx + 1) maxIdx

{-
- Called by monster program code. The move will be adjusted for slopes and
- stairs, but if the move isn't possible, no move is done, false is
- returned, and pr_global_struct.trace_normal is set to the normal of the
- blocking wall.
-}

-- FIXME: since we need to test end position contents here, can we avoid
-- doing it again later in catagorize position?
moveStep :: Ref EdictT -> V3 Float -> Bool -> Quake Bool
moveStep edictRef move relink = do
    -- try the move
    edict <- readRef edictRef

    let oldOrg = edict^.eEntityState.esOrigin
        newOrg = oldOrg + move

    -- flying monsters don't step up
    if (edict^.eFlags) .&. (Constants.flSwim .|. Constants.flFly) /= 0
      then do
        done <- doFlyingStep 0 1

        case done of
          Nothing -> return False
          Just v -> return v

      else do
        -- push down from a step height above the wished position
        let stepSize = if (edict^.eMonsterInfo.miAIFlags) .&. Constants.aiNoStep == 0
                         then fromIntegral $ Constants.stepSize
                         else 1
            newOrg' = _z %~ (+ stepSize) $ newOrg
            end = _z %~ (subtract (stepSize * 2)) $ newOrg'

        trace <- use $ gameBaseGlobals.gbGameImport.giTrace
        traceT <- trace newOrg'
                        (Just $ edict^.eMins)
                        (Just $ edict^.eMaxs)
                        end
                        (Just edictRef)
                        Constants.maskMonsterSolid

        done <- checkAllSolid traceT
                  >>= checkStartSolid edict traceT newOrg' end stepSize
                  >>= checkWaterLevel edict
                  >>= checkAnotherFraction edict
                  >>= checkBottom oldOrg

        case done of
          (Just v, _, _) -> return v
          (Nothing, traceT', _) -> do
            edict' <- readRef edictRef
            when ((edict'^.eFlags) .&. Constants.flPartialGround /= 0) $
              modifyRef edictRef (\v -> v & eFlags %~ (.&. (complement Constants.flPartialGround)))

            modifyRef edictRef (\v -> v & eGroundEntity .~ (traceT^.tEnt))
            let Just traceEntRef = traceT'^.tEnt
            traceEnt <- readRef traceEntRef
            modifyRef edictRef (\v -> v & eGroundEntityLinkCount .~ traceEnt^.eLinkCount)

            -- the move is ok
            when relink $ do
              linkEntity <- use $ gameBaseGlobals.gbGameImport.giLinkEntity
              linkEntity edictRef
              GameBase.touchTriggers edictRef

            return True

  where doFlyingStep :: Int -> Int -> Quake (Maybe Bool)
        doFlyingStep idx maxIdx
          | idx >= maxIdx = return Nothing
          | otherwise = do
              edict <- readRef edictRef
              gameImport <- use $ gameBaseGlobals.gbGameImport

              let newOrg = (edict^.eEntityState.esOrigin) + move
                  trace = gameImport^.giTrace

              newOrg' <- updateNewOrg idx edict newOrg

              traceT <- trace (edict^.eEntityState.esOrigin)
                              (Just $ edict^.eMins)
                              (Just $ edict^.eMaxs)
                              newOrg'
                              (Just edictRef)
                              Constants.maskMonsterSolid

              done <- checkFlyingMonsters edict traceT
                      >>= checkSwimmingMonsters edict traceT
                      >>= checkFraction traceT

              case done of
                Just _ -> return done
                Nothing -> do
                  if isNothing (edict^.eEnemy)
                    then return Nothing
                    else doFlyingStep (idx + 1) maxIdx

        updateNewOrg :: Int -> EdictT -> V3 Float -> Quake (V3 Float)
        updateNewOrg idx edict newOrg = do
          if idx == 0 && isJust (edict^.eEnemy)
            then do
              goalEntity <- if isNothing (edict^.eGoalEntity)
                              then do
                                modifyRef edictRef (\v -> v & eGoalEntity .~ (edict^.eEnemy))
                                let Just goalEntityRef = edict^.eEnemy
                                goalEntity <- readRef goalEntityRef
                                return goalEntity

                              else do
                                let Just goalEntityRef = edict^.eGoalEntity
                                goalEntity <- readRef goalEntityRef
                                return goalEntity

              let dz = (edict^.eEntityState.esOrigin._z) - (goalEntity^.eEntityState.esOrigin._z)

              if isJust (goalEntity^.eClient)
                then do
                  let newOrg' = if dz > 40 then (_z %~ (subtract 8) $ newOrg) else newOrg
                  if not ((edict^.eFlags) .&. Constants.flSwim /= 0 && (edict^.eWaterLevel) < 2) && dz < 30
                    then return (_z %~ (+ 8) $ newOrg')
                    else return newOrg'
                else do
                  return $ if | dz > 8 -> _z %~ (subtract 8) $ newOrg
                              | dz > 0 -> _z %~ (subtract dz) $ newOrg
                              | dz < (-8) -> _z %~ (+ 8) $ newOrg
                              | otherwise -> _z %~ (+ dz) $ newOrg

            else
              return newOrg

        -- fly monsters don't enter water voluntarily
        checkFlyingMonsters :: EdictT -> TraceT -> Quake (Maybe Bool)
        checkFlyingMonsters edict traceT = do
          if (edict^.eFlags) .&. Constants.flFly /= 0 && (edict^.eWaterLevel) == 0
            then do
              let test = _z %~ (+ ((edict^.eMins._z) + 1)) $ traceT^.tEndPos
              pointContents <- use $ gameBaseGlobals.gbGameImport.giPointContents
              contents <- pointContents test
              return $ if contents .&. Constants.maskWater /= 0
                         then Just False
                         else Nothing
            else
              return Nothing

        -- swim monsters don't exit water voluntarily
        checkSwimmingMonsters :: EdictT -> TraceT -> Maybe Bool -> Quake (Maybe Bool)
        checkSwimmingMonsters _ _ done@(Just _) = return done
        checkSwimmingMonsters edict traceT _ = do
          if (edict^.eFlags) .&. Constants.flSwim /= 0 && (edict^.eWaterLevel) < 2
            then do
              let test = _z %~ (+ ((edict^.eMins._z) + 1)) $ traceT^.tEndPos
              pointContents <- use $ gameBaseGlobals.gbGameImport.giPointContents
              contents <- pointContents test
              return $ if contents .&. Constants.maskWater == 0
                         then Just False
                         else Nothing
            else
              return Nothing

        checkFraction :: TraceT -> Maybe Bool -> Quake (Maybe Bool)
        checkFraction _ done@(Just _) = return done
        checkFraction traceT _ = do
          if (traceT^.tFraction) == 1
            then do
              modifyRef edictRef (\v -> v & eEntityState.esOrigin .~ traceT^.tEndPos)

              when relink $ do
                linkEntity <- use $ gameBaseGlobals.gbGameImport.giLinkEntity
                linkEntity edictRef
                GameBase.touchTriggers edictRef

              return (Just True)
            else
              return Nothing

        checkAllSolid :: TraceT -> Quake (Maybe Bool)
        checkAllSolid traceT =
          return $ if traceT^.tAllSolid
                     then Just False
                     else Nothing

        checkStartSolid :: EdictT -> TraceT -> V3 Float -> V3 Float -> Float -> Maybe Bool -> Quake (Maybe Bool, TraceT, V3 Float)
        checkStartSolid _ traceT newOrg _ _ done@(Just _) = return (done, traceT, newOrg)
        checkStartSolid edict traceT newOrg end stepSize _ = do
          if traceT^.tStartSolid
            then do
              let newOrg' = _z %~ (subtract stepSize) $ newOrg
              trace <- use $ gameBaseGlobals.gbGameImport.giTrace
              traceT' <- trace newOrg'
                               (Just $ edict^.eMins)
                               (Just $ edict^.eMaxs)
                               end
                               (Just edictRef)
                               Constants.maskMonsterSolid
              return $ if (traceT'^.tAllSolid) || (traceT'^.tStartSolid)
                         then (Just False, traceT', newOrg')
                         else (Nothing, traceT', newOrg')
            else
              return (Nothing, traceT, newOrg)

        -- don't go in to water
        checkWaterLevel :: EdictT -> (Maybe Bool, TraceT, V3 Float) -> Quake (Maybe Bool, TraceT, V3 Float)
        checkWaterLevel _ done@((Just _), _, _) = return done
        checkWaterLevel edict (_, traceT, newOrg) = do
          if (edict^.eWaterLevel) == 0
            then do
              let test = _z %~ (+ ((edict^.eMins._z) + 1)) $ traceT^.tEndPos
              pointContents <- use $ gameBaseGlobals.gbGameImport.giPointContents
              contents <- pointContents test

              return $ if contents .&. Constants.maskWater /= 0
                         then (Just False, traceT, newOrg)
                         else (Nothing, traceT, newOrg)
            else
              return (Nothing, traceT, newOrg)

        checkAnotherFraction :: EdictT -> (Maybe Bool, TraceT, V3 Float) -> Quake (Maybe Bool, TraceT, V3 Float)
        checkAnotherFraction _ done@(Just _, _, _) = return done
        checkAnotherFraction edict (_, traceT, newOrg) = do
          if (traceT^.tFraction) == 1
            then do
              -- if monster had the ground pulled out, go ahead and fall
              if (edict^.eFlags) .&. Constants.flPartialGround /= 0
                then do
                  modifyRef edictRef (\v -> v & eEntityState.esOrigin +~ move)

                  when relink $ do
                    linkEntity <- use $ gameBaseGlobals.gbGameImport.giLinkEntity
                    linkEntity edictRef
                    GameBase.touchTriggers edictRef
                  
                  modifyRef edictRef (\v -> v & eGroundEntity .~ Nothing)
                  return (Just True, traceT, newOrg)
                else
                  return (Just False, traceT, newOrg) -- walked off an edge
            else
              return (Nothing, traceT, newOrg)

        checkBottom :: V3 Float -> (Maybe Bool, TraceT, V3 Float) -> Quake (Maybe Bool, TraceT, V3 Float)
        checkBottom _ done@(Just _, _, _) = return done
        checkBottom oldOrg (_, traceT, newOrg) = do
          modifyRef edictRef (\v -> v & eEntityState.esOrigin .~ traceT^.tEndPos)

          ok <- M.checkBottom edictRef

          if not ok
            then do
              edict <- readRef edictRef

              if (edict^.eFlags) .&. Constants.flPartialGround /= 0
                then do
                  -- entity had floor mostly pulled out from underneath
                  -- it and is trying to correct
                  when relink $ do
                    linkEntity <- use $ gameBaseGlobals.gbGameImport.giLinkEntity
                    linkEntity edictRef
                    GameBase.touchTriggers edictRef

                  return (Just True, traceT, newOrg)
                else do
                  modifyRef edictRef (\v -> v & eEntityState.esOrigin .~ oldOrg)
                  return (Just False, traceT, newOrg)
            else
              return (Nothing, traceT, newOrg)

{-
- Turns to the movement direction, and walks the current distance if facing
- it.
-}
stepDirection :: Ref EdictT -> Float -> Float -> Quake Bool
stepDirection edictRef yaw dist = do
    -- io (print "SV.stepDirection")
    -- io (print $ "yaw = " ++ show yaw ++ " dist = " ++ show dist)
    modifyRef edictRef (\v -> v & eIdealYaw .~ yaw)
    M.changeYaw edictRef

    let yaw' = yaw * pi  * 2 / 360
        move = V3 ((cos yaw') * dist) ((sin yaw') * dist) 0

    oldOrigin <- readRef edictRef >>= \e -> return (e^.eEntityState.esOrigin)
    moveDone <- moveStep edictRef move False
    linkEntity <- use $ gameBaseGlobals.gbGameImport.giLinkEntity

    if moveDone
      then do
        edict <- readRef edictRef
        let delta = (edict^.eEntityState.esAngles.(Math3D.v3Access Constants.yaw)) - (edict^.eIdealYaw)

        when (delta > 45 && delta < 315) $ -- not turned far enough, so don't take the step
          modifyRef edictRef (\v -> v & eEntityState.esOrigin .~ oldOrigin)

        linkEntity edictRef
        GameBase.touchTriggers edictRef
        return True

      else do
        linkEntity edictRef
        GameBase.touchTriggers edictRef
        return False

{-
- SV_CloseEnough - returns true if distance between 2 ents is smaller than
- given dist.  
-}
closeEnough :: Ref EdictT -> Ref EdictT -> Float -> Quake Bool
closeEnough edictRef goalRef dist = do
    edict <- readRef edictRef
    goal <- readRef goalRef

    if | (goal^.eAbsMin._x) > (edict^.eAbsMax._x) + dist -> return False
       | (goal^.eAbsMin._y) > (edict^.eAbsMax._y) + dist -> return False
       | (goal^.eAbsMin._z) > (edict^.eAbsMax._z) + dist -> return False
       | (goal^.eAbsMax._x) < (edict^.eAbsMin._x) - dist -> return False
       | (goal^.eAbsMax._y) < (edict^.eAbsMin._y) - dist -> return False
       | (goal^.eAbsMax._z) < (edict^.eAbsMin._z) - dist -> return False
       | otherwise -> return True

newChaseDir :: Ref EdictT -> Maybe (Ref EdictT) -> Float -> Quake ()
newChaseDir actorRef maybeEnemyRef dist = do
    -- FIXME: how did we get here with no enemy
    case maybeEnemyRef of
      Nothing -> do
        Com.dprintf "SV_NewChaseDir without enemy!\n"

      Just enemyRef -> do
        actor <- readRef actorRef
        enemy <- readRef enemyRef

        let tmp :: Int = truncate $ (actor^.eIdealYaw) / 45
            oldDir = Math3D.angleMod (fromIntegral $ tmp * 45)
            turnAround = Math3D.angleMod (oldDir - 180)

            deltaX = (enemy^.eEntityState.esOrigin._x) - (actor^.eEntityState.esOrigin._x)
            deltaY = (enemy^.eEntityState.esOrigin._y) - (actor^.eEntityState.esOrigin._y)

            a = if | deltaX > 10 -> 0
                   | deltaX < -10 -> 180
                   | otherwise -> diNoDir

            b = if | deltaY < -10 -> 270
                   | deltaY > 10 -> 90
                   | otherwise -> diNoDir

            d = V3 0 a b

        maybeTDir <- tryDirectRoute turnAround d

        case maybeTDir of
          Nothing -> return ()
          Just _ -> tryOtherDirections actorRef dist oldDir turnAround deltaX deltaY d

  where tryDirectRoute :: Float -> V3 Float -> Quake (Maybe Float)
        tryDirectRoute turnAround d = do
          if (d^._y) /= diNoDir && (d^._z) /= diNoDir
            then do
              let tdir = if (d^._y) == 0
                           then if (d^._z) == 90 then 45 else 315
                           else if (d^._z) == 90 then 135 else 215

              if tdir /= turnAround
                then do
                  v <- stepDirection actorRef tdir dist
                  return $ if v then Just tdir else Nothing
                else
                  return Nothing
            else return Nothing

tryOtherDirections :: Ref EdictT -> Float -> Float -> Float -> Float -> Float -> V3 Float -> Quake ()
tryOtherDirections actorRef dist oldDir turnAround deltaX deltaY d = do
  r <- Lib.rand
  let d' = if (r .&. 3) .&. 1 /= 0 || abs deltaY > abs deltaX
             then V3 (d^._x) (d^._z) (d^._y)
             else d

  if (d'^._y) /= diNoDir && (d'^._y) /= turnAround
    then do
      ok <- stepDirection actorRef (d'^._y) dist
      unless ok $ tryD2Direction d'
    else do
      tryD2Direction d'

  where tryD2Direction :: V3 Float -> Quake ()
        tryD2Direction d' = do
          if (d'^._z) /= diNoDir && (d'^._z) /= turnAround
            then do
              ok <- stepDirection actorRef (d'^._z) dist
              unless ok tryOldDirDirection
            else
              tryOldDirDirection

        -- there is no direct path to the player, so pick another direction
        tryOldDirDirection :: Quake ()
        tryOldDirDirection = do
          if oldDir /= diNoDir
            then do
              ok <- stepDirection actorRef oldDir dist
              unless ok determineSearchDirection
            else
              determineSearchDirection

        determineSearchDirection :: Quake ()
        determineSearchDirection = do
          -- randomly determine direction of search
          r <- Lib.rand
          if r .&. 1 /= 0
            then do
              ok <- tryFromBeginning 0 315
              unless ok tryTurnAroundDirection
            else do
              ok <- tryFromEnd 315 0
              unless ok tryTurnAroundDirection

        tryFromBeginning :: Float -> Float -> Quake Bool
        tryFromBeginning tdir maxTDir
          | tdir > maxTDir = return False
          | otherwise = do
              if tdir /= turnAround
                then do
                  ok <- stepDirection actorRef tdir dist
                  if ok
                    then return True
                    else tryFromBeginning (tdir + 45) maxTDir
                else
                  tryFromBeginning (tdir + 45) maxTDir

        tryFromEnd :: Float -> Float -> Quake Bool
        tryFromEnd tdir minTDir
          | tdir < 0 = return False
          | otherwise = do
              if tdir /= turnAround
                then do
                  ok <- stepDirection actorRef tdir dist
                  if ok
                    then return True
                    else tryFromEnd (tdir - 45) minTDir
                else
                  tryFromEnd (tdir - 45) minTDir

        tryTurnAroundDirection :: Quake ()
        tryTurnAroundDirection = do
          if turnAround /= diNoDir
            then do
              ok <- stepDirection actorRef turnAround dist
              unless ok $ cannotMove
            else
              cannotMove

        cannotMove :: Quake ()
        cannotMove = do
          modifyRef actorRef (\v -> v & eIdealYaw .~ oldDir) -- can't move

          -- if a bridge was pulled out from underneath a monster, it may
          -- not have a valid standing position at all
          ok <- M.checkBottom actorRef
          unless ok $ fixCheckBottom actorRef

fixCheckBottom :: Ref EdictT -> Quake ()
fixCheckBottom edictRef =
    modifyRef edictRef (\v -> v & eFlags %~ (.|. Constants.flPartialGround))
