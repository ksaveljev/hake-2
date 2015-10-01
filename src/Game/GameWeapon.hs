{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Game.GameWeapon where

import Control.Lens (use, preuse, ix, (.=), (^.), zoom, (%=), (&), (.~), (%~))
import Control.Monad (when, liftM, unless)
import Data.Bits ((.|.), (.&.), complement)
import Data.Maybe (isJust, fromJust)
import Linear (V3(..), normalize, norm, _x)

import Quake
import QuakeState
import CVarVariables
import Game.Adapters
import qualified Constants
import {-# SOURCE #-} qualified Game.GameCombat as GameCombat
import qualified Game.GameUtil as GameUtil
import qualified Game.PlayerWeapon as PlayerWeapon
import qualified Util.Lib as Lib
import qualified Util.Math3D as Math3D

blasterTouch :: EntTouch
blasterTouch =
  GenericEntTouch "blaster_touch" $ \selfRef otherRef plane maybeSurf -> do
    self <- readEdictT selfRef

    unless (Just otherRef == (self^.eOwner)) $ do
      if isJust maybeSurf && ((fromJust maybeSurf)^.csFlags) .&. Constants.surfSky /= 0
        then
          GameUtil.freeEdict selfRef
        else do
          let Just ownerRef = self^.eOwner
          owner <- readEdictT ownerRef

          when (isJust (owner^.eClient)) $
            PlayerWeapon.playerNoise ownerRef (self^.eEntityState.esOrigin) Constants.pNoiseImpact

          other <- readEdictT otherRef

          if (other^.eTakeDamage) /= 0
            then do
              let mod' = if self^.eSpawnFlags .&. 1 /= 0
                           then Constants.modHyperblaster
                           else Constants.modBlaster

                  normal = plane^.cpNormal -- TODO: jake2 has check for NULL here

              GameCombat.damage otherRef selfRef ownerRef (self^.eVelocity) (self^.eEntityState.esOrigin) normal (self^.eDmg) 1 Constants.damageEnergy mod'

            else do
              gameImport <- use $ gameBaseGlobals.gbGameImport

              let writeByte = gameImport^.giWriteByte
                  writePosition = gameImport^.giWritePosition
                  writeDir = gameImport^.giWriteDir
                  multicast = gameImport^.giMulticast

              writeByte Constants.svcTempEntity
              writeByte Constants.teBlaster
              writePosition (self^.eEntityState.esOrigin)
              writeDir (plane^.cpNormal) -- TODO: jake2 has check for NULL here
              multicast (self^.eEntityState.esOrigin) Constants.multicastPvs

          GameUtil.freeEdict selfRef

fireHit :: EdictReference -> V3 Float -> Int -> Int -> Quake Bool
fireHit _ _ _ _ = do
    io (putStrLn "GameWeapon.fireHit") >> undefined -- TODO

{-
- ================= 
- fire_blaster
- 
- Fires a single blaster bolt. Used by the blaster and hyper blaster.
- =================
-}
fireBlaster :: EdictReference -> V3 Float -> V3 Float -> Int -> Int -> Int -> Bool -> Quake ()
fireBlaster selfRef start direction damage speed effect hyper = do
    self <- readEdictT selfRef
    let dir = normalize direction

    boltRef <- GameUtil.spawn
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let linkEntity = gameImport^.giLinkEntity
        trace = gameImport^.giTrace
        modelIndex = gameImport^.giModelIndex
        soundIndex = gameImport^.giSoundIndex

    -- yes, I know it looks weird that projectiles are deadmonsters
    -- what this means is that when prediction is used against the object
    -- (blaster/hyperblaster shots), the player won't be solid clipped
    -- against the object. Right now trying to run into a firing hyperblaster
    -- is very jerky since you are predicted 'against' the shots.
    modelIdx <- modelIndex (Just "models/objects/laser/tris.md2")
    soundIdx <- soundIndex (Just "misc/lasfly.wav")
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    modifyEdictT boltRef (\v -> v & eSvFlags .~ Constants.svfDeadMonster
                                  & eEntityState.esOrigin .~ start
                                  & eEntityState.esOldOrigin .~ start
                                  & eEntityState.esAngles .~ dir
                                  & eVelocity .~ fmap (* (fromIntegral speed)) dir
                                  & eMoveType .~ Constants.moveTypeFlyMissile
                                  & eClipMask .~ Constants.maskShot
                                  & eSolid .~ Constants.solidBbox
                                  & eEntityState.esEffects %~ (.|. effect)
                                  & eMins .~ V3 0 0 0
                                  & eMaxs .~ V3 0 0 0
                                  & eEntityState.esModelIndex .~ modelIdx
                                  & eEntityState.esSound .~ soundIdx
                                  & eOwner .~ Just selfRef
                                  & eTouch .~ Just blasterTouch
                                  & eNextThink .~ levelTime + 2
                                  & eThink .~ Just GameUtil.freeEdictA
                                  & eDmg .~ damage
                                  & eClassName .~ "bolt")

    when hyper $
      modifyEdictT boltRef (\v -> v & eSpawnFlags .~ 1)

    linkEntity boltRef

    when (isJust (self^.eClient)) $
      checkDodge selfRef start dir speed

    traceT <- trace (self^.eEntityState.esOrigin) Nothing Nothing start (Just boltRef) Constants.maskShot

    when ((traceT^.tFraction) < 1.0) $ do
      modifyEdictT boltRef (\v -> v & eEntityState.esOrigin .~ start + fmap (* (-10)) dir)
      dummyPlane <- use $ gameBaseGlobals.gbDummyPlane
      touch blasterTouch boltRef (fromJust $ traceT^.tEnt) dummyPlane Nothing

{-
- ================= 
- fire_shotgun
- 
- Shoots shotgun pellets. Used by shotgun and super shotgun.
- =================
-}
fireShotgun :: EdictReference -> V3 Float -> V3 Float -> Int -> Int -> Int -> Int -> Int -> Int -> Quake ()
fireShotgun selfRef start aimDir damage kick hspread vspread count mod'
  | count == 0 = return ()
  | otherwise = do
      fireLead selfRef start aimDir damage kick Constants.teShotgun hspread vspread mod'
      fireShotgun selfRef start aimDir damage kick hspread vspread (count - 1) mod'

fireRail :: EdictReference -> V3 Float -> V3 Float -> Int -> Int -> Quake ()
fireRail _ _ _ _ _ = do
    io (putStrLn "GameWeapon.fireRail") >> undefined -- TODO

{-
- ================= 
- check_dodge
- 
- This is a support routine used when a client is firing a non-instant
- attack weapon. It checks to see if a monster's dodge function should be
- called. 
- =================
-}
checkDodge :: EdictReference -> V3 Float -> V3 Float -> Int -> Quake ()
checkDodge selfRef start dir speed = do
    skillValue <- liftM (^.cvValue) skillCVar

    -- easy mode only ducks one quarter of the time
    r <- Lib.randomF

    unless (skillValue == 0 && r > 0.25) $ do
      let end = start + fmap (* 8192) dir

      trace <- use $ gameBaseGlobals.gbGameImport.giTrace
      traceT <- trace start Nothing Nothing end (Just selfRef) Constants.maskShot

      when (isJust (traceT^.tEnt)) $ do
        let Just traceEntRef = traceT^.tEnt
        traceEnt <- readEdictT traceEntRef
        self <- readEdictT selfRef

        when ((traceEnt^.eSvFlags) .&. Constants.svfMonster /= 0 && (traceEnt^.eHealth) > 0 && isJust (traceEnt^.eMonsterInfo.miDodge) && GameUtil.inFront traceEnt self) $ do
          let v = (traceT^.tEndPos) - start
              eta = (norm v - (traceEnt^.eMaxs._x)) / fromIntegral speed

          dodge (fromJust $ traceEnt^.eMonsterInfo.miDodge) (fromJust $ traceT^.tEnt) selfRef eta

fireLead :: EdictReference -> V3 Float -> V3 Float -> Int -> Int -> Int -> Int -> Int -> Int -> Quake ()
fireLead selfRef start aimDir damage kick impact hspread vspread mod' = do
    self <- readEdictT selfRef
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let contentMask = Constants.maskShot .|. Constants.maskWater
        trace = gameImport^.giTrace
        pointContents = gameImport^.giPointContents

    traceT <- trace (self^.eEntityState.esOrigin) Nothing Nothing start (Just selfRef) Constants.maskShot

    if not ((traceT^.tFraction) < 1.0)
      then do
        let dir = Math3D.vectorAngles aimDir
            (Just forward, Just right, Just up) = Math3D.angleVectors dir True True True

        r <- liftM (* fromIntegral hspread) Lib.crandom
        u <- liftM (* fromIntegral vspread) Lib.crandom

        let end = start + fmap (* 8192) forward
                        + fmap (* r) right
                        + fmap (* u) up

        pc <- pointContents start
        let (water, waterStart, contentMask') = if pc .&. Constants.maskWater /= 0
                                                  then (True, start, contentMask .&. (complement Constants.maskWater))
                                                  else (False, V3 0 0 0, contentMask)

        traceT' <- trace start Nothing Nothing end (Just selfRef) contentMask'

        -- see if we hit water
        if (traceT'^.tContents) .&. Constants.maskWater /= 0
          then
            hitWater traceT' start end
          else do
            sendGunPuffAndFlash traceT' water
            waterBubbleTrail traceT' waterStart water

      else do
        sendGunPuffAndFlash traceT False
        waterBubbleTrail traceT (V3 0 0 0) False

  where sendGunPuffAndFlash :: TraceT -> Bool -> Quake ()
        sendGunPuffAndFlash traceT water = do
          when (not (isJust (traceT^.tSurface) && ((fromJust (traceT^.tSurface))^.csFlags) .&. Constants.surfSky /= 0)) $ do
            when ((traceT^.tFraction) < 1.0) $ do
              let Just traceEntRef = traceT^.tEnt
              traceEnt <- readEdictT traceEntRef

              if (traceEnt^.eTakeDamage) /= 0
                then
                  GameCombat.damage traceEntRef selfRef selfRef aimDir (traceT^.tEndPos) (traceT^.tPlane.cpNormal) damage kick Constants.damageBullet mod'
                else do
                  when (((fromJust (traceT^.tSurface))^.csName) /= "sky") $ do
                    gameImport <- use $ gameBaseGlobals.gbGameImport

                    let writeByte = gameImport^.giWriteByte
                        writePosition = gameImport^.giWritePosition
                        writeDir = gameImport^.giWriteDir
                        multicast = gameImport^.giMulticast

                    writeByte Constants.svcTempEntity
                    writeByte impact
                    writePosition (traceT^.tEndPos)
                    writeDir (traceT^.tPlane.cpNormal)
                    multicast (traceT^.tEndPos) Constants.multicastPvs

                    self <- readEdictT selfRef
                    when (isJust (self^.eClient)) $
                      PlayerWeapon.playerNoise selfRef (traceT^.tEndPos) Constants.pNoiseImpact

        waterBubbleTrail :: TraceT -> V3 Float -> Bool -> Quake ()
        waterBubbleTrail _ _ False = return ()
        waterBubbleTrail traceT waterStart _ = do
          let dir = normalize ((traceT^.tEndPos) - waterStart)
              pos = (traceT^.tEndPos) + fmap (* (-2)) dir

          gameImport <- use $ gameBaseGlobals.gbGameImport
          let pointContents = gameImport^.giPointContents
              trace = gameImport^.giTrace
              writeByte = gameImport^.giWriteByte
              writePosition = gameImport^.giWritePosition
              multicast = gameImport^.giMulticast

          pc <- pointContents pos

          traceT' <- if pc .&. Constants.maskWater /= 0
                       then return traceT { _tEndPos = pos }
                       else trace pos Nothing Nothing waterStart (traceT^.tEnt) Constants.maskWater

          let pos' = fmap (* 0.5) (waterStart + (traceT'^.tEndPos))

          writeByte Constants.svcTempEntity
          writeByte Constants.teBubbleTrail
          writePosition waterStart
          writePosition (traceT'^.tEndPos)
          multicast pos' Constants.multicastPvs

        hitWater :: TraceT -> V3 Float -> V3 Float -> Quake ()
        hitWater traceT start end = do
          let water = True
              waterStart = traceT^.tEndPos

          gameImport <- use $ gameBaseGlobals.gbGameImport
          let writeByte = gameImport^.giWriteByte
              writePosition = gameImport^.giWritePosition
              writeDir = gameImport^.giWriteDir
              multicast = gameImport^.giMulticast
              trace = gameImport^.giTrace

          if start /= waterStart
            then do
              let color = if | (traceT^.tContents) .&. Constants.contentsWater /= 0 ->
                                 if ((fromJust (traceT^.tSurface))^.csName) == "*brwater"
                                   then Constants.splashBrownWater
                                   else Constants.splashBlueWater
                             | (traceT^.tContents) .&. Constants.contentsSlime /= 0 ->
                                 Constants.splashSlime
                             | (traceT^.tContents) .&. Constants.contentsLava /= 0 ->
                                 Constants.splashLava
                             | otherwise ->
                                 Constants.splashUnknown

              when (color /= Constants.splashUnknown) $ do
                writeByte Constants.svcTempEntity
                writeByte Constants.teSplash
                writeByte 8
                writePosition (traceT^.tEndPos)
                writeDir (traceT^.tPlane.cpNormal)
                writeByte color
                multicast (traceT^.tEndPos) Constants.multicastPvs

              -- change bullet's course when it enters water
              let dir = Math3D.vectorAngles (end - start)
                  (Just forward, Just right, Just up) = Math3D.angleVectors dir True True True

              r <- liftM (* (fromIntegral hspread * 2)) Lib.crandom
              u <- liftM (* (fromIntegral vspread * 2)) Lib.crandom

              let end' = waterStart + fmap (* 8192) forward
                                    + fmap (* r) right
                                    + fmap (* u) up

              -- re-trace ignoring water this time
              traceT' <- trace waterStart Nothing Nothing end' (Just selfRef) Constants.maskShot
              sendGunPuffAndFlash traceT' water
              waterBubbleTrail traceT' waterStart water

            else do
              traceT' <- trace waterStart Nothing Nothing end (Just selfRef) Constants.maskShot
              sendGunPuffAndFlash traceT' water
              waterBubbleTrail traceT' waterStart water

{-
- ================= fire_bullet
- 
- Fires a single round. Used for machinegun and chaingun. Would be fine for
- pistols, rifles, etc.... =================
-}
fireBullet :: EdictReference -> V3 Float -> V3 Float -> Int -> Int -> Int -> Int -> Int -> Quake ()
fireBullet selfRef start aimDir damage kick hspread vspread mod =
    fireLead selfRef start aimDir damage kick Constants.teGunshot hspread vspread mod
