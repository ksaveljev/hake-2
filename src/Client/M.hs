{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Client.M where

import Control.Lens ((^.), use, (+=), (%~), (&), (.~), (+~), (-~))
import Control.Monad (unless, when, void)
import Data.Bits ((.|.), (.&.), complement)
import Data.Maybe (isNothing, fromJust, isJust)
import Linear (V3(..), _x, _y, _z)
import qualified Data.Vector as V

import Types
import QuakeState
import Game.Adapters
import qualified Constants
import {-# SOURCE #-} qualified Game.GameCombat as GameCombat
import {-# SOURCE #-} qualified Server.SV as SV
import qualified Util.Lib as Lib
import qualified Util.Math3D as Math3D

checkGround :: EdictReference -> Quake ()
checkGround edictRef = do
    edict <- readEdictT edictRef

    unless ((edict^.eFlags) .&. (Constants.flSwim .|. Constants.flFly) /= 0) $ do
      if (edict^.eVelocity._z) > 100
        then
          modifyEdictT edictRef (\v -> v { _eGroundEntity = Nothing })

        else do
          -- if the hull point one-quarter unit down is solid the entity is
          -- on ground
          let point = (edict^.eEntityState.esOrigin) & _z -~ 0.25

          trace <- use $ gameBaseGlobals.gbGameImport.giTrace
          traceT <- trace (edict^.eEntityState.esOrigin) 
                         (Just $ edict^.eMins)
                         (Just $ edict^.eMaxs)
                         point
                         (Just edictRef)
                         Constants.maskMonsterSolid

          -- check steepness
          if (traceT^.tPlane.cpNormal._z) < 0.7 && not (traceT^.tStartSolid)
            then
              modifyEdictT edictRef (\v -> v { _eGroundEntity = Nothing })

            else do
              when (not (traceT^.tStartSolid) && not (traceT^.tAllSolid)) $ do
                let Just traceEntRef = traceT^.tEnt
                traceEnt <- readEdictT traceEntRef

                modifyEdictT edictRef (\v -> v & eEntityState.esOrigin .~ (traceT^.tEndPos)
                                               & eGroundEntity .~ (traceT^.tEnt)
                                               & eGroundEntityLinkCount .~ (traceEnt^.eLinkCount)
                                               & eVelocity._z .~ 0)

-- Stops the Flies.
fliesOff :: EntThink
fliesOff =
  GenericEntThink "m_fliesoff" $ \edictRef -> do
    modifyEdictT edictRef (\v -> v & eEntityState.esEffects %~ (.&. (complement Constants.efFlies))
                                   & eEntityState.esSound .~ 0)

    return True

-- Starts the Flies as setting the animation flag in the entity.
fliesOn :: EntThink
fliesOn =
  GenericEntThink "m_flies_on" $ \edictRef -> do
    edict <- readEdictT edictRef

    unless ((edict^.eWaterLevel) /= 0) $ do
      levelTime <- use $ gameBaseGlobals.gbLevel.llTime
      soundIndex <- use $ gameBaseGlobals.gbGameImport.giSoundIndex
      soundIdx <- soundIndex (Just "infantry/inflies1.wav")

      modifyEdictT edictRef (\v -> v & eEntityState.esEffects %~ (.|. Constants.efFlies)
                                     & eEntityState.esSound .~ soundIdx
                                     & eThink .~ Just fliesOff
                                     & eNextThink .~ levelTime + 60)

    return True

-- Adds some flies after a random time
flyCheck :: EntThink
flyCheck =
  GenericEntThink "m_fly_check" $ \edictRef -> do
    edict <- readEdictT edictRef

    f <- Lib.randomF

    unless ((edict^.eWaterLevel /= 0) || f > 0.5) $ do
      levelTime <- use $ gameBaseGlobals.gbLevel.llTime
      nf <- Lib.randomF

      modifyEdictT edictRef (\v -> v & eThink .~ Just fliesOn
                                     & eNextThink .~ levelTime + 5 + 10 * nf)

    return True

dropToFloor :: EntThink
dropToFloor =
  GenericEntThink "m_drop_to_floor" $ \edictRef -> do
    modifyEdictT edictRef (\v -> v & eEntityState.esOrigin._z +~ 1)

    edict <- readEdictT edictRef
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let trace = gameImport^.giTrace
        linkEntity = gameImport^.giLinkEntity
        end' = (edict^.eEntityState.esOrigin) & _z -~ 256

    traceT <- trace (edict^.eEntityState.esOrigin)
                    (Just $ edict^.eMins)
                    (Just $ edict^.eMaxs)
                    end'
                    (Just edictRef)
                    Constants.maskMonsterSolid

    if (traceT^.tFraction) == 1 || (traceT^.tAllSolid)
      then
        return True

      else do
        modifyEdictT edictRef (\v -> v & eEntityState.esOrigin .~ (traceT^.tEndPos))

        linkEntity edictRef
        checkGround edictRef
        catagorizePosition edictRef

        return True

{-
- Returns false if any part of the bottom of the entity is off an edge that
- is not a staircase.
-}
checkBottom :: EdictReference -> Quake Bool
checkBottom edictRef = do
    edict <- readEdictT edictRef

    let mins = (edict^.eEntityState.esOrigin) + (edict^.eMins)
        maxs = (edict^.eEntityState.esOrigin) + (edict^.eMaxs)

    -- if all of the points under the corners are solid world, don't bother
    -- with the tougher checks
    -- the corners must be within 16 of the midpoint
    done <- doChecks ((mins^._z) - 1) mins maxs 0 2 0 2

    case done of
      Just v -> return v

      Nothing -> do
        gameBaseGlobals.gbCYes += 1
        return True -- we got out easy

  where doChecks :: Float -> V3 Float -> V3 Float -> Int -> Int -> Int -> Int -> Quake (Maybe Bool)
        doChecks c mins maxs x maxX y maxY
          | x >= maxX = return Nothing
          | y >= maxY = doChecks c mins maxs (x + 1) maxX 0 maxY
          | otherwise = do
              let a = if x /= 0 then maxs^._x else mins^._x
                  b = if y /= 0 then maxs^._y else mins^._y
                  start = V3 a b c

              gameImport <- use $ gameBaseGlobals.gbGameImport

              let pointContents = gameImport^.giPointContents
                  trace = gameImport^.giTrace

              contents <- pointContents start

              if contents /= Constants.contentsSolid
                then do
                  gameBaseGlobals.gbCNo += 1
                  -- check it for real
                  let a' = ((mins^._x) + (maxs^._x)) * 0.5
                      b' = ((mins^._y) + (maxs^._y)) * 0.5
                      start' = V3 a' b' (mins^._z)
                      stop' = V3 a' b' ((mins^._z) - 2 * (fromIntegral Constants.stepSize))

                  v3o <- use $ globals.vec3Origin
                  traceT <- trace start' (Just v3o) (Just v3o) stop' (Just edictRef) Constants.maskMonsterSolid

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

        checkCorners :: V3 Float -> V3 Float -> V3 Float -> V3 Float -> Float -> Float -> Int -> Int -> Int -> Int -> Quake (Maybe Bool)
        checkCorners mins maxs start stop mid bottom x maxX y maxY
          | x >= maxX = return Nothing
          | y >= maxY = checkCorners mins maxs start stop mid bottom (x + 1) maxX 0 maxY
          | otherwise = do
              let a = if x /= 0 then maxs^._x else mins^._x
                  b = if y /= 0 then maxs^._y else mins^._y
                  start' = V3 a b (start^._z)
                  stop' = V3 a b (stop^._z)

              v3o <- use $ globals.vec3Origin
              trace <- use $ gameBaseGlobals.gbGameImport.giTrace
              traceT <- trace start' (Just v3o) (Just v3o) stop' (Just edictRef) Constants.maskMonsterSolid

              let bottom' = if (traceT^.tFraction) /= 1 && (traceT^.tEndPos._z) > bottom
                              then traceT^.tEndPos._z
                              else bottom

              if (traceT^.tFraction) == 1 || mid - (traceT^.tEndPos._z) > fromIntegral Constants.stepSize
                then return (Just False)
                else checkCorners mins maxs start' stop' mid bottom' x maxX (y + 1) maxY

walkMove :: EdictReference -> Float -> Float -> Quake Bool
walkMove edictRef yaw dist = do
    edict <- readEdictT edictRef

    if isNothing (edict^.eGroundEntity) && (edict^.eFlags) .&. (Constants.flFly .|. Constants.flSwim) == 0
      then
        return False

      else do
        let yaw' = yaw * pi * 2 / 360
            move = V3 ((cos yaw') * dist) ((sin yaw') * dist) 0

        SV.moveStep edictRef move True

catagorizePosition :: EdictReference -> Quake ()
catagorizePosition edictRef = do
    edict <- readEdictT edictRef
    pointContents <- use $ gameBaseGlobals.gbGameImport.giPointContents

    let point = (edict^.eEntityState.esOrigin) & _z +~ (edict^.eMins._z) + 1
    cont <- pointContents point

    if cont .&. Constants.maskWater == 0
      then do
        modifyEdictT edictRef (\v -> v & eWaterLevel .~ 0
                                       & eWaterType .~ 0)

      else do
        modifyEdictT edictRef (\v -> v & eWaterLevel .~ 1
                                       & eWaterType .~ cont)

        let point' = point & _z +~ 26
        cont' <- pointContents point'

        unless (cont' .&. Constants.maskWater == 0) $ do
          modifyEdictT edictRef (\v -> v & eWaterLevel .~ 2)
          let point'' = point' & _z +~ 22
          cont'' <- pointContents point''

          when (cont'' .&. Constants.maskWater /= 0) $
            modifyEdictT edictRef (\v -> v & eWaterLevel .~ 3)

moveFrame :: EdictReference -> Quake ()
moveFrame selfRef = do
    self <- readEdictT selfRef

    levelTime <- use $ gameBaseGlobals.gbLevel.llTime
    modifyEdictT selfRef (\v -> v & eNextThink .~ levelTime + Constants.frameTime)

    let Just move = self^.eMonsterInfo.miCurrentMove

    -- io (print "BEFORE")
    -- io (print $ "self.frame = " ++ show (self^.eEntityState.esFrame))
    -- io (print $ "move.firstframe = " ++ show (move^.mmFirstFrame))

    done <- if (self^.eMonsterInfo.miNextFrame) /= 0 && (self^.eMonsterInfo.miNextFrame) >= (move^.mmFirstFrame) && (self^.eMonsterInfo.miNextFrame) <= (move^.mmLastFrame)
              then do
                modifyEdictT selfRef (\v -> v & eEntityState.esFrame .~ (self^.eMonsterInfo.miNextFrame)
                                              & eMonsterInfo.miNextFrame .~ 0)

                return False
              else do
                done <- if (self^.eEntityState.esFrame) == move^.mmLastFrame && isJust (move^.mmEndFunc)
                          then do
                            void $ think (fromJust $ move^.mmEndFunc) selfRef

                            self' <- readEdictT selfRef

                            -- check for death
                            if (self'^.eSvFlags) .&. Constants.svfDeadMonster /= 0
                              then return True
                              else return False

                          else
                            return False

                if done
                  then
                    return True

                  else do
                    -- regrab move, endfunc is very likely to change it
                    self' <- readEdictT selfRef
                    let Just move' = self'^.eMonsterInfo.miCurrentMove

                    if (self'^.eEntityState.esFrame) < (move'^.mmFirstFrame) || (self'^.eEntityState.esFrame) > (move'^.mmLastFrame)
                      then do
                        modifyEdictT selfRef (\v -> v & eMonsterInfo.miAIFlags %~ (.&. (complement Constants.aiHoldFrame))
                                                      & eEntityState.esFrame .~ (move'^.mmFirstFrame))

                        return False

                      else do
                        when ((self'^.eMonsterInfo.miAIFlags) .&. Constants.aiHoldFrame == 0) $ do
                          if (self'^.eEntityState.esFrame) + 1 > move'^.mmLastFrame
                            then
                              modifyEdictT selfRef (\v -> v & eEntityState.esFrame .~ (move'^.mmFirstFrame))
                            else
                              modifyEdictT selfRef (\v -> v & eEntityState.esFrame +~ 1)

                        return False

    unless done $ do
      -- regrab move, endfunc is very likely to change it
      self' <- readEdictT selfRef
      let Just move' = self'^.eMonsterInfo.miCurrentMove
          index = (self'^.eEntityState.esFrame) - (move'^.mmFirstFrame)
          frame = (move'^.mmFrame) V.! index

      -- io (print "AFTER")
      -- io (print $ "self.frame = " ++ show (self'^.eEntityState.esFrame))
      -- io (print $ "move.firstframe = " ++ show (move'^.mmFirstFrame))
      -- io (print $ "INDEX = " ++ show index)

      when (isJust (frame^.mfAI)) $ do
        -- io (print "YUP, calling AI")
        if (self'^.eMonsterInfo.miAIFlags) .&. Constants.aiHoldFrame == 0
          then
            ai (fromJust $ frame^.mfAI) selfRef ((frame^.mfDist) * (self'^.eMonsterInfo.miScale))
          else
            ai (fromJust $ frame^.mfAI) selfRef 0

      when (isJust (frame^.mfThink)) $
        void $ think (fromJust $ frame^.mfThink) selfRef

worldEffects :: EdictReference -> Quake ()
worldEffects edictRef = do
    checkWaterDamage

    edict <- readEdictT edictRef

    if edict^.eWaterLevel == 0
      then do
        when ((edict^.eFlags) .&. Constants.flInWater /= 0) $ do
          gameImport <- use $ gameBaseGlobals.gbGameImport

          let sound = gameImport^.giSound
              soundIndex = gameImport^.giSoundIndex

          soundIdx <- soundIndex (Just "player/watr_out.wav")
          sound (Just edictRef) Constants.chanBody soundIdx 1 Constants.attnNorm 0

          modifyEdictT edictRef (\v -> v & eFlags %~ (.&. (complement Constants.flInWater)))

      else do
        checkLava
        checkSlime
        checkInWater

  where checkWaterDamage :: Quake ()
        checkWaterDamage = do
          edict <- readEdictT edictRef

          when ((edict^.eHealth) > 0) $ do
            levelTime <- use $ gameBaseGlobals.gbLevel.llTime

            -- IMPROVE: refactor, too much same stuff here
            if (edict^.eFlags) .&. Constants.flSwim == 0
              then if | (edict^.eWaterLevel) < 3 ->
                          modifyEdictT edictRef (\v -> v & eAirFinished .~ levelTime + 12)

                      | (edict^.eAirFinished) < levelTime -> do
                          -- drown!
                          when ((edict^.ePainDebounceTime) < levelTime) $ do
                            let dmg :: Int = 2 + 2 * floor (levelTime - (edict^.eAirFinished))
                                dmg' = if dmg > 15 then 15 else dmg

                            v3o <- use $ globals.vec3Origin
                            GameCombat.damage edictRef worldRef worldRef v3o (edict^.eEntityState.esOrigin) v3o dmg' 0 Constants.damageNoArmor Constants.modWater

                            modifyEdictT edictRef (\v -> v & ePainDebounceTime .~ levelTime + 1)

                      | otherwise -> return ()

              else if | (edict^.eWaterLevel) > 0 ->
                          modifyEdictT edictRef (\v -> v & eAirFinished .~ levelTime + 9)

                      | (edict^.eAirFinished) < levelTime -> do
                          -- suffocate!
                          when ((edict^.ePainDebounceTime) < levelTime) $ do
                            let dmg :: Int = 2 + 2 * floor (levelTime - (edict^.eAirFinished))
                                dmg' = if dmg > 15 then 15 else dmg

                            v3o <- use $ globals.vec3Origin
                            GameCombat.damage edictRef worldRef worldRef v3o (edict^.eEntityState.esOrigin) v3o dmg' 0 Constants.damageNoArmor Constants.modWater

                            modifyEdictT edictRef (\v -> v & ePainDebounceTime .~ levelTime + 1)

                      | otherwise -> return ()

        checkLava :: Quake ()
        checkLava = do
          edict <- readEdictT edictRef

          when ((edict^.eWaterType) .&. Constants.contentsLava /= 0 && (edict^.eFlags) .&. Constants.flImmuneLava == 0) $ do
            levelTime <- use $ gameBaseGlobals.gbLevel.llTime

            when ((edict^.eDamageDebounceTime) < levelTime) $ do
              v3o <- use $ globals.vec3Origin
              GameCombat.damage edictRef worldRef worldRef v3o (edict^.eEntityState.esOrigin) v3o (10 * (edict^.eWaterLevel)) 0 0 Constants.modLava

        checkSlime :: Quake ()
        checkSlime = do
          edict <- readEdictT edictRef

          when ((edict^.eWaterType) .&. Constants.contentsSlime /= 0 && (edict^.eFlags) .&. Constants.flImmuneSlime == 0) $ do
            levelTime <- use $ gameBaseGlobals.gbLevel.llTime

            when ((edict^.eDamageDebounceTime) < levelTime) $ do
              v3o <- use $ globals.vec3Origin
              GameCombat.damage edictRef worldRef worldRef v3o (edict^.eEntityState.esOrigin) v3o (4 * (edict^.eWaterLevel)) 0 0 Constants.modSlime

        checkInWater :: Quake ()
        checkInWater = do
          edict <- readEdictT edictRef

          when ((edict^.eFlags) .&. Constants.flInWater == 0) $ do
            when ((edict^.eSvFlags) .&. Constants.svfDeadMonster == 0) $ do
              gameImport <- use $ gameBaseGlobals.gbGameImport

              let sound = gameImport^.giSound
                  soundIndex = gameImport^.giSoundIndex

              if | (edict^.eWaterType) .&. Constants.contentsLava /= 0 -> do
                     r <- Lib.randomF
                     soundIdx <- if r <= 0.5
                                   then soundIndex (Just "player/lava1.wav")
                                   else soundIndex (Just "player/lava2.wav")
                     sound (Just edictRef) Constants.chanBody soundIdx 1 Constants.attnNorm 0

                 | (edict^.eWaterType) .&. Constants.contentsSlime /= 0 -> do
                     soundIdx <- soundIndex (Just "player/watr_in.wav")
                     sound (Just edictRef) Constants.chanBody soundIdx 1 Constants.attnNorm 0

                 | (edict^.eWaterType) .&. Constants.contentsWater /= 0 -> do
                     soundIdx <- soundIndex (Just "player/watr_in.wav")
                     sound (Just edictRef) Constants.chanBody soundIdx 1 Constants.attnNorm 0

                 | otherwise -> return ()

            modifyEdictT edictRef (\v -> v & eFlags %~ (.|. Constants.flInWater)
                                           & eDamageDebounceTime .~ 0)

setEffects :: EdictReference -> Quake ()
setEffects edictRef = do
    modifyEdictT edictRef (\v -> v & eEntityState.esEffects %~ (.&. (complement (Constants.efColorShell .|. Constants.efPowerScreen)))
                                   & eEntityState.esRenderFx %~ (.&. (complement (Constants.rfShellRed .|. Constants.rfShellGreen .|. Constants.rfShellBlue))))

    edict <- readEdictT edictRef

    when ((edict^.eMonsterInfo.miAIFlags) .&. Constants.aiResurrecting /= 0) $ do
      modifyEdictT edictRef (\v -> v & eEntityState.esEffects %~ (.|. Constants.efColorShell)
                                     & eEntityState.esRenderFx %~ (.|. Constants.rfShellRed))

    unless ((edict^.eHealth) <= 0) $ do
      levelTime <- use $ gameBaseGlobals.gbLevel.llTime

      when ((edict^.ePowerArmorTime) > levelTime) $
        if | (edict^.eMonsterInfo.miPowerArmorType) == Constants.powerArmorScreen ->
               modifyEdictT edictRef (\v -> v & eEntityState.esEffects %~ (.|. Constants.efPowerScreen))

           | (edict^.eMonsterInfo.miPowerArmorType) == Constants.powerArmorShield ->
               modifyEdictT edictRef (\v -> v & eEntityState.esEffects %~ (.|. Constants.efColorShell)
                                              & eEntityState.esRenderFx %~ (.|. Constants.rfShellGreen))

           | otherwise -> return ()

changeYaw :: EdictReference -> Quake ()
changeYaw edictRef = do
    edict <- readEdictT edictRef

    let current = Math3D.angleMod (edict^.eEntityState.esAngles.(Math3D.v3Access Constants.yaw))
        ideal = edict^.eIdealYaw

    unless (current == ideal) $ do
      let move = ideal - current
          speed = edict^.eYawSpeed
          move' = if ideal > current
                    then if move >= 180 then move - 360 else move
                    else if move <= -180 then move + 360 else move
          move'' = if move' > 0
                     then if move' > speed then speed else move'
                     else if move' < -speed then -speed else move'

          access = case Constants.yaw of
                     0 -> _x
                     1 -> _y
                     2 -> _z
                     _ -> undefined -- shouldn't happen

      modifyEdictT edictRef (\v -> v & eEntityState.esAngles.access .~ Math3D.angleMod (current + move''))

moveToGoal :: EdictReference -> Float -> Quake ()
moveToGoal edictRef dist = do
    edict <- readEdictT edictRef

    let skip = isNothing (edict^.eGroundEntity) && ((edict^.eFlags) .&. (Constants.flFly .|. Constants.flSwim) == 0)
    -- if the next step hits the enemy, return immediately
    skip' <- if isJust (edict^.eEnemy)
               then SV.closeEnough edictRef (fromJust $ edict^.eEnemy) dist
               else return False

    -- io (print "M.moveToGoal")
    -- io (print $ "skip = " ++ show skip ++ " skip' = " ++ show skip')

    unless (skip || skip') $ do
      edict' <- readEdictT edictRef

      -- bump around
      r <- Lib.rand

      if r .&. 3 == 1
        then do
          when (edict'^.eInUse) $
            SV.newChaseDir edictRef (edict'^.eGoalEntity) dist
        else do
          v <- SV.stepDirection edictRef (edict'^.eIdealYaw) dist

          unless v $ do
            edict'' <- readEdictT edictRef

            when (edict''^.eInUse) $
              SV.newChaseDir edictRef (edict''^.eGoalEntity) dist
