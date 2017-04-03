{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Game.GameWeapon where

import Control.Lens (use, preuse, ix, (.=), (^.), zoom, (%=), (&), (.~), (%~))
import Control.Monad (when, liftM, unless, void)
import Data.Bits ((.|.), (.&.), complement)
import Data.Maybe (isJust, isNothing, fromJust)
import Linear (V3(..), normalize, norm, _x, _y, _z)

import Game.EdictT
import Game.MonsterInfoT
import Types
import Game.CSurfaceT
import QuakeRef
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
    self <- readRef selfRef

    unless (Just otherRef == (self^.eOwner)) $ do
      if isJust maybeSurf && ((fromJust maybeSurf)^.csFlags) .&. Constants.surfSky /= 0
        then
          GameUtil.freeEdict selfRef
        else do
          let Just ownerRef = self^.eOwner
          owner <- readRef ownerRef

          when (isJust (owner^.eClient)) $
            PlayerWeapon.playerNoise ownerRef (self^.eEntityState.esOrigin) Constants.pNoiseImpact

          other <- readRef otherRef

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

{-
- ================= 
- fire_hit
- 
- Used for all impact (hit/punch/slash) attacks 
- =================
-}
fireHit :: Ref EdictT -> V3 Float -> Int -> Int -> Quake Bool
fireHit selfRef aim damage kick = do
    -- see if enemy is in range
    self <- readRef selfRef
    let Just enemyRef = self^.eEnemy
    enemy <- readRef enemyRef
    
    let dir = (enemy^.eEntityState.esOrigin) - (self^.eEntityState.esOrigin)
        range = norm dir
    
    if range > (aim^._x)
      then
        return False
      else do
        let (range', aim') = if (aim^._y) > (self^.eMins._x) && (aim^._y) < (self^.eMaxs._x)
                               -- the hit is straight on so back the range up to the edge of their bbox
                               then (range - (enemy^.eMaxs._x), aim)
                               -- this is a side hit so adjust the "right" value out to the edge of their bbox
                               else if (aim^._y) < 0
                                      then (range, aim & _y .~ (enemy^.eMins._x))
                                      else (range, aim & _y .~ (enemy^.eMaxs._x))
        
            point = (self^.eEntityState.esOrigin) + fmap (* range') dir
        
        trace <- use $ gameBaseGlobals.gbGameImport.giTrace
        traceT <- trace (self^.eEntityState.esOrigin) Nothing Nothing point (Just selfRef) Constants.maskShot
        
        mTraceT <- if (traceT^.tFraction) < 1
                     then do
                       let Just traceEntRef = traceT^.tEnt
                       traceEnt <- readRef traceEntRef
                       
                       if (traceEnt^.eTakeDamage) == 0
                         then
                           return Nothing -- we are done
                         else
                           -- if it will hit any client/monster then hit the one we wanted to hit
                           if (traceEnt^.eSvFlags) .&. Constants.svfMonster /= 0 || isJust (traceEnt^.eClient)
                             then return (Just (traceT & tEnt .~ (self^.eEnemy)))
                             else return (Just traceT)
                     else
                       return (Just traceT)
        
        case mTraceT of
          Nothing ->
            return False
          
          Just traceT' -> do
            let (Just forward, Just right, Just up) = Math3D.angleVectors (self^.eEntityState.esAngles) True True True
                point' = (self^.eEntityState.esOrigin) + fmap (* range') forward
                                                       + fmap (* (aim'^._y)) right
                                                       + fmap (* (aim'^._z)) up
                dir' = point' - enemy^.eEntityState.esOrigin
            
            -- do the damage
            v3o <- use $ globals.gVec3Origin
            let Just traceEntRef = traceT'^.tEnt
            GameCombat.damage traceEntRef selfRef selfRef dir' point' v3o damage (kick `div` 2) Constants.damageNoKnockback Constants.modHit
            
            traceEnt <- readRef traceEntRef
            if (traceEnt^.eSvFlags) .&. Constants.svfMonster == 0 && isNothing (traceEnt^.eClient)
              then
                return False
              
              else do
                -- do our special form of knockback here
                let a = (enemy^.eAbsMin) + fmap (* 0.5) (enemy^.eSize)
                    a' = normalize (a - point')
                    velocity = (enemy^.eVelocity) + fmap (* (fromIntegral kick)) a'
                  
                modifyRef enemyRef (\v -> v & eVelocity .~ velocity)
                
                when ((velocity^._z) > 0) $
                  modifyRef enemyRef (\v -> v & eGroundEntity .~ Nothing)
                
                return True

{-
- ================= 
- fire_blaster
- 
- Fires a single blaster bolt. Used by the blaster and hyper blaster.
- =================
-}
fireBlaster :: Ref EdictT -> V3 Float -> V3 Float -> Int -> Int -> Int -> Bool -> Quake ()
fireBlaster selfRef start direction damage speed effect hyper = do
    self <- readRef selfRef
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

    modifyRef boltRef (\v -> v & eSvFlags .~ Constants.svfDeadMonster
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
      modifyRef boltRef (\v -> v & eSpawnFlags .~ 1)

    linkEntity boltRef

    when (isJust (self^.eClient)) $
      checkDodge selfRef start dir speed

    traceT <- trace (self^.eEntityState.esOrigin) Nothing Nothing start (Just boltRef) Constants.maskShot

    when ((traceT^.tFraction) < 1.0) $ do
      modifyRef boltRef (\v -> v & eEntityState.esOrigin .~ start + fmap (* (-10)) dir)
      dummyPlane <- use $ gameBaseGlobals.gbDummyPlane
      touch blasterTouch boltRef (fromJust $ traceT^.tEnt) dummyPlane Nothing

{-
- ================= 
- fire_shotgun
- 
- Shoots shotgun pellets. Used by shotgun and super shotgun.
- =================
-}
fireShotgun :: Ref EdictT -> V3 Float -> V3 Float -> Int -> Int -> Int -> Int -> Int -> Int -> Quake ()
fireShotgun selfRef start aimDir damage kick hspread vspread count mod'
  | count == 0 = return ()
  | otherwise = do
      fireLead selfRef start aimDir damage kick Constants.teShotgun hspread vspread mod'
      fireShotgun selfRef start aimDir damage kick hspread vspread (count - 1) mod'

fireRail :: Ref EdictT -> V3 Float -> V3 Float -> Int -> Int -> Quake ()
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
checkDodge :: Ref EdictT -> V3 Float -> V3 Float -> Int -> Quake ()
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
        traceEnt <- readRef traceEntRef
        self <- readRef selfRef

        when ((traceEnt^.eSvFlags) .&. Constants.svfMonster /= 0 && (traceEnt^.eHealth) > 0 && isJust (traceEnt^.eMonsterInfo.miDodge) && GameUtil.inFront traceEnt self) $ do
          let v = (traceT^.tEndPos) - start
              eta = (norm v - (traceEnt^.eMaxs._x)) / fromIntegral speed

          dodge (fromJust $ traceEnt^.eMonsterInfo.miDodge) (fromJust $ traceT^.tEnt) selfRef eta

fireLead :: Ref EdictT -> V3 Float -> V3 Float -> Int -> Int -> Int -> Int -> Int -> Int -> Quake ()
fireLead selfRef start aimDir damage kick impact hspread vspread mod' = do
    self <- readRef selfRef
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
              traceEnt <- readRef traceEntRef

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

                    self <- readRef selfRef
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
- ================= fire_bullet =================
- 
- Fires a single round. Used for machinegun and chaingun. Would be fine for
- pistols, rifles, etc....
-}
fireBullet :: Ref EdictT -> V3 Float -> V3 Float -> Int -> Int -> Int -> Int -> Int -> Quake ()
fireBullet selfRef start aimDir damage kick hspread vspread mod =
    fireLead selfRef start aimDir damage kick Constants.teGunshot hspread vspread mod

fireGrenade :: Ref EdictT -> V3 Float -> V3 Float -> Int -> Int -> Float -> Float -> Quake ()
fireGrenade selfRef start aimDir damage speed timer damageRadius = do
    let dir = Math3D.vectorAngles aimDir
        (Just forward, Just right, Just up) = Math3D.angleVectors dir True True True
    
    grenadeRef <- GameUtil.spawn
    c1 <- Lib.crandom
    c2 <- Lib.crandom
    let velocity = fmap (* (fromIntegral speed)) aimDir + fmap (* (200 + c1 * 10)) up + fmap (* (c2 * 10)) right
    
    gameImport <- use $ gameBaseGlobals.gbGameImport
    let modelIndex = gameImport^.giModelIndex
        linkEntity = gameImport^.giLinkEntity
    
    modelIdx <- modelIndex (Just "models/objects/grenade/tris.md2")
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime
    
    modifyRef grenadeRef (\v -> v & eEntityState.esOrigin .~ start
                                     & eVelocity .~ velocity
                                     & eAVelocity .~ V3 300 300 300
                                     & eMoveType .~ Constants.moveTypeBounce
                                     & eClipMask .~ Constants.maskShot
                                     & eSolid .~ Constants.solidBbox
                                     & eEntityState.esEffects %~ (\a -> a .|. Constants.efGrenade)
                                     & eMins .~ V3 0 0 0
                                     & eMaxs .~ V3 0 0 0
                                     & eEntityState.esModelIndex .~ modelIdx
                                     & eOwner .~ Just selfRef
                                     & eTouch .~ Just grenadeTouch
                                     & eNextThink .~ levelTime + timer
                                     & eThink .~ Just grenadeExplode
                                     & eDmg .~ damage
                                     & eDmgRadius .~ damageRadius
                                     & eClassName .~ "grenade"
                                     )
    
    linkEntity grenadeRef

fireRocket :: Ref EdictT -> V3 Float -> V3 Float -> Int -> Int -> Float -> Int -> Quake ()
fireRocket selfRef start dir damage speed damageRadius radiusDamage = do
    gameImport <- use $ gameBaseGlobals.gbGameImport
    let modelIndex = gameImport^.giModelIndex
        soundIndex = gameImport^.giSoundIndex
        linkEntity = gameImport^.giLinkEntity
    
    rocketRef <- GameUtil.spawn
    modelIdx <- modelIndex (Just "models/objects/rocket/tris.md2")
    soundIdx <- soundIndex (Just "weapons/rockfly.wav")
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime
    
    modifyRef rocketRef (\v -> v & eEntityState.esOrigin .~ start
                                    & eMoveDir .~ dir
                                    & eEntityState.esAngles .~ dir
                                    & eVelocity .~ fmap (* (fromIntegral speed)) dir
                                    & eMoveType .~ Constants.moveTypeFlyMissile
                                    & eClipMask .~ Constants.maskShot
                                    & eSolid .~ Constants.solidBbox
                                    & eEntityState.esEffects %~ (\a -> a .|. Constants.efRocket)
                                    & eMins .~ V3 0 0 0
                                    & eMaxs .~ V3 0 0 0
                                    & eEntityState.esModelIndex .~ modelIdx
                                    & eOwner .~ Just selfRef
                                    & eTouch .~ Just rocketTouch
                                    & eNextThink .~ levelTime + 8000 / (fromIntegral speed)
                                    & eThink .~ Just GameUtil.freeEdictA
                                    & eDmg .~ damage
                                    & eRadiusDmg .~ radiusDamage
                                    & eDmgRadius .~ damageRadius
                                    & eEntityState.esSound .~ soundIdx
                                    & eClassName .~ "rocket"
                                    )
    
    self <- readRef selfRef
    case self^.eClient of
      Nothing -> return ()
      Just _ -> checkDodge selfRef start dir speed
    
    linkEntity rocketRef

fireBFG :: Ref EdictT -> V3 Float -> V3 Float -> Int -> Int -> Float -> Quake ()
fireBFG selfRef start dir damage speed damageRadius = do
    gameImport <- use $ gameBaseGlobals.gbGameImport
    let modelIndex = gameImport^.giModelIndex
        soundIndex = gameImport^.giSoundIndex
        linkEntity = gameImport^.giLinkEntity
    
    bfgRef <- GameUtil.spawn
    modelIdx <- modelIndex (Just "sprites/s_bfg1.sp2")
    soundIdx <- soundIndex (Just "weapons/bfg__l1a.wav")
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime
    
    modifyRef bfgRef (\v -> v & eEntityState.esOrigin .~ start
                                 & eMoveDir .~ dir
                                 & eEntityState.esAngles .~ Math3D.vectorAngles dir
                                 & eVelocity .~ fmap (* (fromIntegral speed)) dir
                                 & eMoveType .~ Constants.moveTypeFlyMissile
                                 & eClipMask .~ Constants.maskShot
                                 & eSolid .~ Constants.solidBbox
                                 & eEntityState.esEffects %~ (\a -> a .|. Constants.efBFG .|. Constants.efAnimAllFast)
                                 & eMins .~ V3 0 0 0
                                 & eMaxs .~ V3 0 0 0
                                 & eEntityState.esModelIndex .~ modelIdx
                                 & eOwner .~ Just selfRef
                                 & eTouch .~ Just bfgTouch
                                 & eNextThink .~ levelTime + Constants.frameTime
                                 & eThink .~ Just bfgThink
                                 & eRadiusDmg .~ damage
                                 & eDmgRadius .~ damageRadius
                                 & eClassName .~ "bfg blast"
                                 & eEntityState.esSound .~ soundIdx
                                 & eTeamMaster .~ Just bfgRef
                                 & eTeamChain .~ Nothing
                                 )
                                 
    
    self <- readRef selfRef
    case self^.eClient of
      Nothing -> return ()
      Just _ -> checkDodge selfRef start dir speed
    
    linkEntity bfgRef

fireGrenade2 :: Ref EdictT -> V3 Float -> V3 Float -> Int -> Int -> Float -> Float -> Bool -> Quake ()
fireGrenade2 selfRef start aimDir damage speed timer damageRadius held = do
    gameImport <- use $ gameBaseGlobals.gbGameImport
    let modelIndex = gameImport^.giModelIndex
        soundIndex = gameImport^.giSoundIndex
        linkEntity = gameImport^.giLinkEntity
        sound = gameImport^.giSound
    
    grenadeRef <- GameUtil.spawn
    c1 <- Lib.crandom
    c2 <- Lib.crandom
    modelIdx <- modelIndex (Just "models/objects/grenade2/tris.md2")
    soundIdx <- soundIndex (Just "weapons/hgrenc1b.wav")
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime
    
    let dir = Math3D.vectorAngles aimDir
        (Just forward, Just right, Just up) = Math3D.angleVectors dir True True True
        velocity = fmap (* (fromIntegral speed)) aimDir + fmap (* (200 + c1 * 10)) up + fmap (* (c2 * 10)) right
    
    modifyRef grenadeRef (\v -> v & eEntityState.esOrigin .~ start
                                     & eVelocity .~ velocity
                                     & eAVelocity .~ V3 300 300 300
                                     & eMoveType .~ Constants.moveTypeBounce
                                     & eClipMask .~ Constants.maskShot
                                     & eSolid .~ Constants.solidBbox
                                     & eEntityState.esEffects %~ (\a -> a .|. Constants.efGrenade)
                                     & eMins .~ V3 0 0 0
                                     & eMaxs .~ V3 0 0 0
                                     & eEntityState.esModelIndex .~ modelIdx
                                     & eOwner .~ Just selfRef
                                     & eTouch .~ Just grenadeTouch
                                     & eNextThink .~ levelTime + timer
                                     & eThink .~ Just grenadeExplode
                                     & eDmg .~ damage
                                     & eDmgRadius .~ damageRadius
                                     & eClassName .~ "hgrenade"
                                     & eSpawnFlags .~ (if held then 3 else 1)
                                     & eEntityState.esSound .~ soundIdx
                                     )
    
    if timer <= 0
      then
        void $ think grenadeExplode grenadeRef
      else do
        soundIdx' <- soundIndex (Just "weapons/hgrent1a.wav")
        sound (Just selfRef) Constants.chanWeapon soundIdx' 1 Constants.attnNorm 0
        linkEntity grenadeRef

grenadeTouch :: EntTouch
grenadeTouch =
  GenericEntTouch "grenade_touch" $ \edictRef otherRef _ mSurf -> do
    edict <- readRef edictRef
    
    unless ((edict^.eOwner) == Just otherRef) $ do
      let done = checkSurf mSurf
      
      if done
        then
          GameUtil.freeEdict edictRef
        
        else do
          other <- readRef otherRef
          
          if (other^.eTakeDamage) == 0
            then do
              gameImport <- use $ gameBaseGlobals.gbGameImport
              let sound = gameImport^.giSound
                  soundIndex = gameImport^.giSoundIndex
              
              soundIdx <- if (edict^.eSpawnFlags) .&. 1 /= 0
                            then do
                              r <- Lib.randomF
                              if r > 0.5
                                then soundIndex (Just "weapons/hgrenb1a.wav")
                                else soundIndex (Just "weapons/hgrenb2a.wav")
                            else
                              soundIndex (Just "weapons/grenlb1b.wav")
                              
              sound (Just edictRef) Constants.chanVoice soundIdx 1 Constants.attnNorm 0
            
            else do
              modifyRef edictRef (\v -> v & eEnemy .~ Just otherRef)
              void $ think grenadeExplode edictRef
              
  where checkSurf :: Maybe CSurfaceT -> Bool
        checkSurf Nothing = False
        checkSurf (Just surf) = (surf^.csFlags) .&. Constants.surfSky /= 0

grenadeExplode :: EntThink
grenadeExplode =
  GenericEntThink "grenade_explode" $ \_ -> do
    io (putStrLn "GameWeapon.grenadeExplode") >> undefined -- TODO

rocketTouch :: EntTouch
rocketTouch =
  GenericEntTouch "rocket_touch" $ \_ _ _ _ -> do
    io (putStrLn "GameWeapon.rocketTouch") >> undefined -- TODO

bfgTouch :: EntTouch
bfgTouch =
  GenericEntTouch "bfg_touch" $ \_ _ _ _ -> do
    io (putStrLn "GameWeapon.bfgTouch") >> undefined -- TODO

bfgThink :: EntThink
bfgThink =
  GenericEntThink "bfg_think" $ \_ -> do
    io (putStrLn "GameWeapon.bfgThink") >> undefined -- TODO
