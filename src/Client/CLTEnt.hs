{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Client.CLTEnt where

import Control.Lens (zoom, (.=), use, (^.), ix)
import Control.Monad (void, when, liftM)
import Data.Bits (shiftR, (.|.), (.&.))
import Data.IORef (newIORef, IORef, readIORef, modifyIORef', writeIORef)
import Data.Maybe (isNothing, fromJust, isJust)
import Linear (V3(..), _x, _y, _z, norm, normalize)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import Quake
import QuakeState
import qualified Constants
import qualified Client.CLFX as CLFX
import qualified Client.CLNewFX as CLNewFX
import {-# SOURCE #-} qualified Client.V as ClientV
import qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import qualified QCommon.MSG as MSG
import qualified Sound.S as S
import qualified Util.Lib as Lib
import qualified Util.Math3D as Math3D

exFree :: Int
exFree = 0

exExplosion :: Int
exExplosion = 1

exMisc :: Int
exMisc = 2

exFlash :: Int
exFlash = 3

exMFlash :: Int
exMFlash = 4

exPoly :: Int
exPoly = 5

exPoly2 :: Int
exPoly2 = 6

splashColor :: UV.Vector Int
splashColor = UV.fromList [ 0x00, 0xe0, 0xb0, 0x50, 0xd0, 0xe0, 0xe8 ]

clearTEnts :: Quake ()
clearTEnts = do
    explosions <- use $ clTEntGlobals.clteExplosions
    io $ V.mapM_ (\exRef -> writeIORef exRef newExplosionT) explosions

    zoom clTEntGlobals $ do
      clteBeams       .= V.replicate Constants.maxBeams newBeamT
      cltePlayerBeams .= V.replicate Constants.maxBeams newBeamT
      clteLasers      .= V.replicate Constants.maxLasers newLaserT
      clteSustains    .= V.replicate Constants.maxSustains newCLSustainT

registerTEntSounds :: Quake ()
registerTEntSounds = do
    sfxRic1      <- S.registerSound "world/ric1.wav"
    sfxRic2      <- S.registerSound "world/ric2.wav"
    sfxRic3      <- S.registerSound "world/ric3.wav"
    sfxLashIt    <- S.registerSound "weapons/lashit.wav"
    sfxSpark5    <- S.registerSound "world/spark5.wav"
    sfxSpark6    <- S.registerSound "world/spark6.wav"
    sfxSpark7    <- S.registerSound "world/spark7.wav"
    sfxRailg     <- S.registerSound "weapons/railgf1a.wav"
    sfxRockExp   <- S.registerSound "weapons/rocklx1a.wav"
    sfxGrenExp   <- S.registerSound "weapons/grenlx1a.wav"
    sfxWatrExp   <- S.registerSound "weapons/xpld_wat.wav"
    sfxFootStep1 <- S.registerSound "player/step1.wav"
    sfxFootStep2 <- S.registerSound "player/step2.wav"
    sfxFootStep3 <- S.registerSound "player/step3.wav"
    sfxFootStep4 <- S.registerSound "player/step4.wav"
    sfxLightning <- S.registerSound "weapons/tesla.wav"
    sfxDisrExp   <- S.registerSound "weapons/disrupthit.wav"

    void $ S.registerSound "player/land1.wav"
    void $ S.registerSound "player/fall2.wav"
    void $ S.registerSound "player/fall1.wav"

    zoom clTEntGlobals $ do
      clteSfxRic1      .= sfxRic1
      clteSfxRic2      .= sfxRic2
      clteSfxRic3      .= sfxRic3
      clteSfxLashIt    .= sfxLashIt
      clteSfxSpark5    .= sfxSpark5
      clteSfxSpark6    .= sfxSpark6
      clteSfxSpark7    .= sfxSpark7
      clteSfxRailg     .= sfxRailg
      clteSfxRockExp   .= sfxRockExp
      clteSfxGrenExp   .= sfxGrenExp
      clteSfxWatrExp   .= sfxWatrExp
      clteSfxFootsteps .= V.fromList [sfxFootStep1, sfxFootStep2, sfxFootStep3, sfxFootStep4]
      clteSfxLightning .= sfxLightning
      clteSfxDisrExp   .= sfxDisrExp

registerTEntModels :: Quake ()
registerTEntModels = do
    Just renderer <- use $ globals.re
    let registerModel = renderer^.rRefExport.reRegisterModel
        registerPic = renderer^.rRefExport.reRegisterPic

    modExplode         <- registerModel "models/objects/explode/tris.md2"
    modSmoke           <- registerModel "models/objects/smoke/tris.md2"
    modFlash           <- registerModel "models/objects/flash/tris.md2"
    modParasiteSegment <- registerModel "models/monsters/parasite/segment/tris.md2"
    modGrappleCable    <- registerModel "models/ctf/segment/tris.md2"
    modParasiteTip     <- registerModel "models/monsters/parasite/tip/tris.md2"
    modExplo4          <- registerModel "models/objects/r_explode/tris.md2"
    modBfgExplo        <- registerModel "sprites/s_bfg2.sp2"
    modPowerScreen     <- registerModel "models/items/armor/effect/tris.md2"

    void $ registerModel "models/objects/laser/tris.md2"
    void $ registerModel "models/objects/grenade2/tris.md2"
    void $ registerModel "models/weapons/v_machn/tris.md2"
    void $ registerModel "models/weapons/v_handgr/tris.md2"
    void $ registerModel "models/weapons/v_shotg2/tris.md2"
    void $ registerModel "models/objects/gibs/bone/tris.md2"
    void $ registerModel "models/objects/gibs/sm_meat/tris.md2"
    void $ registerModel "models/objects/gibs/bone2/tris.md2"
    -- void $ registerModel "models/objects/blaser/tris.md2"

    void $ registerPic "w_machinegun"
    void $ registerPic "a_bullets"
    void $ registerPic "i_health"
    void $ registerPic "a_grenades"

    modExplo4Big       <- registerModel "models/objects/r_explode2/tris.md2"
    modLightning       <- registerModel "models/proj/lightning/tris.md2"
    modHeatBeam        <- registerModel "models/proj/beam/tris.md2"
    modMonsterHeatBeam <- registerModel "models/proj/widowbeam/tris.md2"

    zoom clTEntGlobals $ do
      clteModExplode         .= modExplode
      clteModSmoke           .= modSmoke
      clteModFlash           .= modFlash
      clteModParasiteSegment .= modParasiteSegment
      clteModGrappleCable    .= modGrappleCable
      clteModParasiteTip     .= modParasiteTip
      clteModExplo4          .= modExplo4
      clteModBfgExplo        .= modBfgExplo
      clteModPowerScreen     .= modPowerScreen
      clteModLightning       .= modLightning
      clteModHeatBeam        .= modHeatBeam
      clteModMonsterHeatBeam .= modMonsterHeatBeam
      clteModExplo4Big       .= modExplo4Big

addTEnts :: Quake ()
addTEnts = do
    addBeams
    -- PMM - draw plasma beams
    addPlayerBeams
    addExplosions
    addLasers
    -- PMM - set up sustain
    processSustain

addBeams :: Quake ()
addBeams = do
    beams <- use $ clTEntGlobals.clteBeams
    cl' <- use $ globals.cl
    addBeam beams cl' 0 Constants.maxBeams

  where addBeam :: V.Vector BeamT -> ClientStateT -> Int -> Int -> Quake ()
        addBeam beams cl' idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              let b = beams V.! idx

              if isNothing (b^.bModel) || (b^.bEndTime) < (cl'^.csTime)
                then
                  addBeam beams cl' (idx + 1) maxIdx
                else do
                  -- if coming from the player, update the start position
                  let start = if (b^.bEntity) == (cl'^.csPlayerNum) + 1 -- entity 0 is the world
                                then let V3 x y z = cl'^.csRefDef.rdViewOrg
                                     in V3 x y (z - 22) -- adjust for view height
                                else b^.bStart
                      org = start + (b^.bOffset)
                      dist = (b^.bEnd) - org
                      (pitch, yaw) = if (dist^._y) == 0 && (dist^._x) == 0
                                       then let pitch = if (dist^._z) > 0 then 90 else 270
                                            in (pitch, 0)
                                       else let yaw = if | (dist^._x) /= 0 -> (atan2 (dist^._y) (dist^._x)) * 180 / pi
                                                         | (dist^._y) > 0 -> 90
                                                         | otherwise -> 270
                                                yaw' = if yaw < 0 then yaw + 360 else yaw
                                                forward = sqrt((dist^._x) * (dist^._x) + (dist^._y) * (dist^._y))
                                                pitch = (atan2 (dist^._z) forward) * (-180) / pi
                                                pitch' = if pitch < 0 then pitch + 360 else pitch
                                            in (pitch', yaw')
                      d = norm dist
                      dist' = normalize dist

                  clTEntGlobals.clteBeams.ix idx.bStart .= start
                  modLightning <- use $ clTEntGlobals.clteModLightning

                  let (modelLength, d') = if (b^.bModel) == modLightning
                                            then (35.0, d - 20) -- correction so it doesn't end in middle of tesla
                                            else (30.0, d)
                      steps = fromIntegral $ ceiling (d' / modelLength)
                      len = (d' - modelLength) / (steps - 1)

                  -- PMM - special case for lightning model .. if the real
                  -- length is shorter than the model,
                  -- flip it around & draw it from the end to the start.
                  -- This prevents the model from going through the tesla
                  -- mine (instead it goes through the target)
                  if (b^.bModel) == modLightning && d' <= modelLength
                    then do
                      r <- Lib.rand
                      ent <- io $ newIORef newEntityT { _eOrigin = b^.bEnd
                                                      , _eModel = b^.bModel
                                                      , _enFlags = Constants.rfFullBright
                                                      , _eAngles = V3 pitch yaw (fromIntegral $ r `mod` 360)
                                                      }

                      ClientV.addEntity ent
                    else do
                      constructBeams b org len modLightning pitch yaw d' dist' modelLength
                      addBeam beams cl' (idx + 1) maxIdx

        constructBeams :: BeamT -> V3 Float -> Float -> Maybe (IORef ModelT) -> Float -> Float -> Float -> V3 Float -> Float -> Quake ()
        constructBeams b org len modLightning pitch yaw d dist modelLength
          | d <= 0 = return ()
          | otherwise = do
              let ent = newEntityT { _eOrigin = org
                                   , _eModel = b^.bModel
                                   }

              r <- Lib.rand

              if (b^.bModel) == modLightning
                then do
                  entRef <- io $ newIORef ent { _enFlags = Constants.rfFullBright
                                              , _eAngles = V3 (-pitch) (yaw + 180) (fromIntegral $ r `mod` 360)
                                              }
                  ClientV.addEntity entRef
                else do
                  entRef <- io $ newIORef ent { _eAngles = V3 pitch yaw (fromIntegral $ r `mod` 360) }
                  ClientV.addEntity entRef

              let org' = org + fmap (* len) dist
              constructBeams b org' len modLightning pitch yaw (d - modelLength) dist modelLength

addPlayerBeams :: Quake ()
addPlayerBeams = do
    handMultiplier <- getHandMultiplier
    beams <- use $ clTEntGlobals.cltePlayerBeams
    cl' <- use $ globals.cl

    addPlayerBeam beams cl' handMultiplier 0 Constants.maxBeams
  
  where getHandMultiplier :: Quake Float
        getHandMultiplier = do
          maybeHand <- CVar.findVar "hand"
          case maybeHand of
            Nothing -> return 1
            Just hand -> return $ if | (hand^.cvValue) == 2 -> 0
                                     | (hand^.cvValue) == 1 -> (-1)
                                     | otherwise -> 1

        addPlayerBeam :: V.Vector BeamT -> ClientStateT -> Float -> Int -> Int -> Quake ()
        addPlayerBeam beams cl' handMultiplier idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              let b = beams V.! idx

              if isNothing (b^.bModel) || (b^.bEndTime) < (cl'^.csTime)
                then
                  addPlayerBeam beams cl' handMultiplier (idx + 1) maxIdx
                else do
                  modLightning <- use $ clTEntGlobals.clteModLightning
                  modHeatBeam <- use $ clTEntGlobals.clteModHeatBeam
                  maybeHand <- CVar.findVar "hand"

                  let (org, start, r, f, u) = if isJust modHeatBeam && (b^.bModel) == modHeatBeam
                                                then
                                                  -- if coming from the player, update the start position
                                                  if (b^.bEntity) == (cl'^.csPlayerNum) + 1 -- entity 0 is the world
                                                         -- set up gun position
                                                    then let ps = cl'^.csFrame.fPlayerState
                                                             j = ((cl'^.csFrame.fServerFrame) - 1) .&. Constants.updateMask
                                                             oldFrame = (cl'^.csFrames) V.! j
                                                             oldFrame' = if (oldFrame^.fServerFrame) /= (cl'^.csFrame.fServerFrame) - 1 || not (oldFrame^.fValid)
                                                                           then cl'^.csFrame -- previous frame was dropped or invalid
                                                                           else oldFrame
                                                             ops = oldFrame'^.fPlayerState
                                                             start = (cl'^.csRefDef.rdViewOrg)
                                                                   + (ops^.psGunOffset)
                                                                   + (fmap (* (cl'^.csLerpFrac)) ((ps^.psGunOffset) - (ops^.psGunOffset)))
                                                             org = start + fmap (* (handMultiplier * (b^.bOffset._x))) (cl'^.csVRight)
                                                             org' = org + fmap (* (b^.bOffset._y)) (cl'^.csVForward)
                                                             org'' = org' + fmap (* (b^.bOffset._z)) (cl'^.csVUp)
                                                             org''' = case maybeHand of
                                                                        Nothing -> org''
                                                                        Just hand -> if (hand^.cvValue) == 2
                                                                                       then org'' + fmap (* (-1)) (cl'^.csVUp)
                                                                                       else org''
                                                         in (org''', start, (cl'^.csVRight), (cl'^.csVForward), (cl'^.csVUp))
                                                    else
                                                      (b^.bStart, b^.bStart, V3 0 0 0, V3 0 0 0, V3 0 0 0)
                                                else
                                                  -- if coming from the player, update the start position
                                                  let start = if (b^.bEntity) == (cl'^.csPlayerNum) + 1 -- entity 0 is the world
                                                                then let V3 x y z = cl'^.csRefDef.rdViewOrg
                                                                     in V3 x y (z - 22) -- adjust for view height
                                                                else b^.bStart
                                                      org = start + (b^.bOffset)
                                                  in (org, start, V3 0 0 0, V3 0 0 0, V3 0 0 0)

                      dist = (b^.bEnd) - org
                      (dist', org') = if isJust modHeatBeam && (b^.bModel) == modHeatBeam && (b^.bEntity) == (cl'^.csPlayerNum) + 1
                                        then let len = norm dist
                                                 dist' = fmap (* len) f
                                                       + fmap (* (handMultiplier * (b^.bOffset._x))) r
                                                       + fmap (* (b^.bOffset._y)) f
                                                       + fmap (* (b^.bOffset._z)) u
                                                 org' = case maybeHand of
                                                          Nothing -> org
                                                          Just hand -> if (hand^.cvValue) == 2
                                                                         then org + fmap (* (-1)) (cl'^.csVUp)
                                                                         else org
                                             in (dist', org')
                                        else (dist, org)

                      (pitch, yaw) = if (dist'^._y) == 0 && (dist'^._x) == 0
                                       then let pitch = if (dist'^._z) > 0 then 90 else 270
                                            in (pitch, 0)
                                       else let yaw = if | (dist'^._x) /= 0 -> (atan2 (dist'^._y) (dist'^._x)) * 180 / pi
                                                         | (dist'^._y) > 0 -> 90
                                                         | otherwise -> 270
                                                yaw' = if yaw < 0 then yaw + 360 else yaw
                                                forward = sqrt((dist'^._x) * (dist'^._x) + (dist'^._y) * (dist'^._y))
                                                pitch = (atan2 (dist'^._z) forward) * (-180) / pi
                                                pitch' = if pitch < 0 then pitch + 360 else pitch
                                            in (pitch', yaw')

                  (frameNum, org'') <- if isJust modHeatBeam && (b^.bModel) == modHeatBeam
                                         then
                                           if (b^.bEntity) /= (cl'^.csPlayerNum) + 1
                                             then do
                                               let angles = V3 (-pitch) (yaw + 180) 0
                                                   (Just f', Just r', Just u') = Math3D.angleVectors angles True True True
                                               v3o <- use $ globals.vec3Origin
         
                                               -- if it's a non-origin offset, it's a player,
                                               -- so use the hardcoded player offset
                                               org'' <- if (b^.bOffset) /= v3o
                                                          then 
                                                            return $ org' + fmap (* (1 - (b^.bOffset._x))) r'
                                                                          + fmap (* (0 - (b^.bOffset._y))) f'
                                                                          + fmap (* (0 - (b^.bOffset._z) - 10)) u'
                                                          else do
                                                            CLNewFX.monsterPlasmaShell start
                                                            return org'
         
                                               return (2, org'')
                                             else
                                               return (1, org')
                                         else
                                           return (0, org')

                  clTEntGlobals.cltePlayerBeams.ix idx.bStart .= start

                  -- if it's the heatbeam, draw the particle effect
                  when (isJust modHeatBeam && (b^.bModel) == modHeatBeam && (b^.bEntity) == (cl'^.csPlayerNum) + 1) $
                    CLNewFX.heatBeam org'' dist'

                  let d = norm dist'
                      dist'' = normalize dist'
                      (d', modelLength) = if | (b^.bModel) == modHeatBeam -> (d, 32.0)
                                             | (b^.bModel) == modLightning -> (d - 20, 35.0) -- correction so it doesn't end in middle of tesla
                                             | otherwise -> (d, 30.0)
                      steps = fromIntegral $ ceiling (d' / modelLength)
                      len = (d' - modelLength) / (steps - 1)
         
                  -- PMM - special case for lightning model .. if the real
                  -- length is shorter than the model,
                  -- flip it around & draw it from the end to the start.
                  -- This prevents the model from going through the tesla
                  -- mine (instead it goes through the target)
                  if (b^.bModel) == modLightning && d' <= modelLength
                    then do
                      r <- Lib.rand
                      entRef <- io $ newIORef newEntityT { _eOrigin = b^.bEnd
                                                         , _eModel = b^.bModel
                                                         , _enFlags = Constants.rfFullBright
                                                         , _eAngles = V3 pitch yaw (fromIntegral $ r `mod` 360)
                                                         }

                      ClientV.addEntity entRef
                    else do
                      constructBeams b org'' len modHeatBeam modLightning pitch yaw d' dist'' modelLength frameNum
                      addPlayerBeam beams cl' handMultiplier (idx + 1) maxIdx

        constructBeams :: BeamT -> V3 Float -> Float -> Maybe (IORef ModelT) -> Maybe (IORef ModelT) -> Float -> Float -> Float -> V3 Float -> Float -> Int -> Quake ()
        constructBeams b org len modHeatBeam modLightning pitch yaw d dist modelLength frameNum
          | d <= 0 = return ()
          | otherwise = do
              let ent = newEntityT { _eOrigin = org
                                   , _eModel = b^.bModel
                                   }

              r <- Lib.rand

              entRef <- io $ if | isJust modHeatBeam && (b^.bModel) == modHeatBeam ->
                                    newIORef ent { _enFlags = Constants.rfFullBright
                                                 , _eAngles = V3 (-pitch) (yaw + 180) (fromIntegral $ r `mod` 360)
                                                 , _eFrame = frameNum
                                                 }
                                | (b^.bModel) == modLightning ->
                                    newIORef ent { _enFlags = Constants.rfFullBright
                                                 , _eAngles = V3 (-pitch) (yaw + 180) (fromIntegral $ r `mod` 360)
                                                 }

                                | otherwise ->
                                    newIORef ent { _eAngles = V3 pitch yaw (fromIntegral $ r `mod` 360) }

              ClientV.addEntity entRef

              let org' = org + fmap (* len) dist
              constructBeams b org' len modHeatBeam modLightning pitch yaw (d - modelLength) dist modelLength frameNum

addExplosions :: Quake ()
addExplosions = do
    explosions <- use $ clTEntGlobals.clteExplosions
    cl' <- use $ globals.cl
    addExplosion explosions cl' 0 Constants.maxExplosions

  where addExplosion :: V.Vector (IORef ExplosionT) -> ClientStateT -> Int -> Int -> Quake ()
        addExplosion explosions cl' idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              let exRef = explosions V.! idx
              ex <- io $ readIORef exRef

              if (ex^.eType) == exFree
                then
                  addExplosion explosions cl' (idx + 1) maxIdx
                else do
                  let frac = (fromIntegral (cl'^.csTime) - (ex^.eStart)) / 100
                      f = truncate frac :: Int

                  maybeEntRef <- if | (ex^.eType) == exMFlash -> do
                                        return $ if f >= (ex^.eFrames) - 1
                                                   then Nothing
                                                   else Just (ex^.eEnt)

                                    | (ex^.eType) == exMisc -> do
                                        if f >= (ex^.eFrames) - 1
                                          then return Nothing
                                          else do
                                            io $ modifyIORef' (ex^.eEnt) (\v -> v { _eAlpha = 1 - frac / fromIntegral ((ex^.eFrames) - 1) })
                                            return (Just (ex^.eEnt))

                                    | (ex^.eType) == exFlash -> do
                                        if f >= 1
                                          then return Nothing
                                          else do
                                            io $ modifyIORef' (ex^.eEnt) (\v -> v { _eAlpha = 1 })
                                            return (Just (ex^.eEnt))

                                    | (ex^.eType) == exPoly -> do
                                        if f >= (ex^.eFrames) - 1
                                          then return Nothing
                                          else do
                                            let alpha = (16.0 - fromIntegral f) / 16
                                                skinNum = if f < 10
                                                            then if (f `shiftR` 1) < 0
                                                                   then 0
                                                                   else f `shiftR` 1
                                                            else if f < 13
                                                                   then 5
                                                                   else 6
                                                flagUpdate = if f < 10
                                                               then 0
                                                               else Constants.rfTranslucent
                                            io $ modifyIORef' (ex^.eEnt) (\v -> v { _eAlpha = alpha
                                                                                  , _eSkinNum = skinNum
                                                                                  , _enFlags = (v^.enFlags) .|. flagUpdate
                                                                                  })
                                            return (Just (ex^.eEnt))

                                    | (ex^.eType) == exPoly2 -> do
                                        if f >= (ex^.eFrames) - 1
                                          then return Nothing
                                          else do
                                            io $ modifyIORef' (ex^.eEnt) (\v -> v { _eAlpha = (5.0 - fromIntegral f) / 5.0
                                                                                  , _eSkinNum = 0
                                                                                  , _enFlags = (v^.enFlags) .|. Constants.rfTranslucent
                                                                                  })
                                            return (Just (ex^.eEnt))

                                    | otherwise -> return Nothing -- shouldn't happend tbh

                  case maybeEntRef of
                    Nothing -> do
                      io $ modifyIORef' exRef (\v -> v { _eType = exFree })
                      addExplosion explosions cl' (idx + 1) maxIdx
                    Just entRef -> do
                      ent <- io $ readIORef entRef

                      when ((ex^.eLight) /= 0) $
                        ClientV.addLight (ent^.eOrigin)
                                         ((ex^.eLight) * (ent^.eAlpha))
                                         (ex^.eLightColor._x)
                                         (ex^.eLightColor._y)
                                         (ex^.eLightColor._z)

                      let f' = if f < 0 then 0 else f

                      io $ modifyIORef' entRef (\v -> v { _eOldOrigin = ent^.eOrigin
                                                        , _eFrame = (ex^.eBaseFrame) + f' + 1
                                                        , _eOldFrame = (ex^.eBaseFrame) + f'
                                                        , _eBackLerp = 1 - (cl'^.csLerpFrac)
                                                        })

                      ClientV.addEntity entRef

                      addExplosion explosions cl' (idx + 1) maxIdx

addLasers :: Quake ()
addLasers = do
    lasers <- use $ clTEntGlobals.clteLasers
    time <- use $ globals.cl.csTime
    addLaser lasers time 0 Constants.maxLasers

  where addLaser :: V.Vector LaserT -> Int -> Int -> Int -> Quake ()
        addLaser lasers time idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              let laser = lasers V.! idx

              when ((laser^.lEndTime) >= time) $
                ClientV.addEntity (laser^.lEnt)

              addLaser lasers time (idx + 1) maxIdx

processSustain :: Quake ()
processSustain = do
    sustains <- use $ clTEntGlobals.clteSustains
    time <- use $ globals.cl.csTime
    process sustains time 0 Constants.maxSustains

  where process :: V.Vector CLSustainT -> Int -> Int -> Int -> Quake ()
        process sustains time idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              let s = sustains V.! idx

              if | (s^.clsEndTime) >= time && time >= (s^.clsNextThink) ->
                     void $ (fromJust $ s^.clsThink) s
                 | (s^.clsEndTime) < time ->
                     clTEntGlobals.clteSustains.ix idx.clsId .= 0
                 | otherwise -> return ()

              process sustains time (idx + 1) maxIdx

parseTEnt :: Quake ()
parseTEnt = do
    entType <- MSG.readByte (globals.netMessage)

         -- bullet hitting flesh
    if | entType == Constants.teBlood -> do
           pos <- MSG.readPos (globals.netMessage)
           dir <- MSG.readDir (globals.netMessage)
           CLFX.particleEffect pos dir 0xE8 60

         -- bullet hitting wall
       | any (== entType) [Constants.teGunshot, Constants.teSparks, Constants.teBulletSparks] -> do
           io (print "CLTEnt.parseTEnt 2") >> undefined -- TODO

       | any (== entType) [Constants.teScreenSparks, Constants.teShieldSparks] -> do
           io (print "CLTEnt.parseTEnt 3") >> undefined -- TODO

         -- bullet hitting wall
       | entType == Constants.teShotgun -> do
           io (print "CLTEnt.parseTEnt 4") >> undefined -- TODO

         -- bullet hitting water
       | entType == Constants.teSplash -> do
           cnt <- MSG.readByte (globals.netMessage)
           pos <- MSG.readPos (globals.netMessage)
           dir <- MSG.readDir (globals.netMessage)
           r <- MSG.readByte (globals.netMessage)
           let color = if r > 6 then 0x00 else splashColor UV.! r

           CLFX.particleEffect pos dir color cnt

           when (r == Constants.splashSparks) $ do
             r' <- liftM (.&. 3) Lib.rand
             sfxRef <- if | r' == 0 -> use $ clTEntGlobals.clteSfxSpark5
                          | r' == 1 -> use $ clTEntGlobals.clteSfxSpark6
                          | otherwise -> use $ clTEntGlobals.clteSfxSpark7
             S.startSound (Just pos) (EdictReference 0) 0 sfxRef 1 Constants.attnStatic 0

       | entType == Constants.teLaserSparks -> do
           io (print "CLTEnt.parseTEnt 6") >> undefined -- TODO

       | entType == Constants.teBlueHyperblaster -> do
           io (print "CLTEnt.parseTEnt 7") >> undefined -- TODO

         -- blaster hitting wall
       | entType == Constants.teBlaster -> do
           io (print "CLTEnt.parseTEnt 8") >> undefined -- TODO

         -- railgun effect
       | entType == Constants.teRailTrail -> do
           io (print "CLTEnt.parseTEnt 9") >> undefined -- TODO

       | any (== entType) [Constants.teExplosion2, Constants.teGrenadeExplosion, Constants.teGrenadeExplosionWater] -> do
           io (print "CLTEnt.parseTEnt 10") >> undefined -- TODO

       | entType == Constants.tePlasmaExplosion -> do
           io (print "CLTEnt.parseTEnt 11") >> undefined -- TODO

       | any (== entType) [Constants.teExplosion1, Constants.teExplosion1Big, Constants.teRocketExplosion, Constants.teRocketExplosionWater, Constants.teExplosion1Np] -> do
           pos <- MSG.readPos (globals.netMessage)

           exRef <- allocExplosion

           r <- liftM (fromIntegral . (`mod` 360)) Lib.rand
           model <- if entType /= Constants.teExplosion1Big
                      then use $ clTEntGlobals.clteModExplo4
                      else use $ clTEntGlobals.clteModExplo4Big

           let ent = newEntityT { _enFlags = Constants.rfFullBright
                                , _eAngles = V3 0 r 0
                                , _eModel  = model
                                }

           entRef <- io $ newIORef ent
           serverTime <- use $ globals.cl.csFrame.fServerTime
           f <- Lib.randomF
           let baseFrame = if f < 0.5 then 15 else 0

           let ex = ExplosionT { _eType       = exPoly
                               , _eEnt        = entRef
                               , _eFrames     = 15
                               , _eLight      = 350
                               , _eLightColor = V3 1.0 0.5 0.5
                               , _eStart      = fromIntegral (serverTime  - 100)
                               , _eBaseFrame  = baseFrame
                               }

           io $ writeIORef exRef ex

           when (entType /= Constants.teExplosion1Big && entType /= Constants.teExplosion1Np) $
             CLFX.explosionParticles pos

           sfxRef <- if entType == Constants.teRocketExplosionWater
                       then use $ clTEntGlobals.clteSfxWatrExp
                       else use $ clTEntGlobals.clteSfxRockExp

           S.startSound (Just pos) (EdictReference 0) 0 sfxRef 1 Constants.attnNorm 0

       | entType == Constants.teBfgExplosion -> do
           io (print "CLTEnt.parseTEnt 13") >> undefined -- TODO

       | entType == Constants.teBfgBigExplosion -> do
           io (print "CLTEnt.parseTEnt 14") >> undefined -- TODO

       | entType == Constants.teBfgLaser -> do
           io (print "CLTEnt.parseTEnt 15") >> undefined -- TODO

       | entType == Constants.teBubbleTrail -> do
           io (print "CLTEnt.parseTEnt 16") >> undefined -- TODO

       | any (== entType) [Constants.teParasiteAttack, Constants.teMedicCableAttack] -> do
           io (print "CLTEnt.parseTEnt 17") >> undefined -- TODO

         -- boss teleporting to station
       | entType == Constants.teBossTPort -> do
           io (print "CLTEnt.parseTEnt 18") >> undefined -- TODO

       | entType == Constants.teGrappleCable -> do
           io (print "CLTEnt.parseTEnt 19") >> undefined -- TODO

       | entType == Constants.teWeldingSparks -> do
           io (print "CLTEnt.parseTEnt 20") >> undefined -- TODO

       | entType == Constants.teGreenBlood -> do
           io (print "CLTEnt.parseTEnt 21") >> undefined -- TODO

       | entType == Constants.teTunnelSparks -> do
           io (print "CLTEnt.parseTEnt 22") >> undefined -- TODO

       | any (== entType) [Constants.teBlaster2, Constants.teFlechette] -> do
           io (print "CLTEnt.parseTEnt 23") >> undefined -- TODO

       | entType == Constants.teLightning -> do
           io (print "CLTEnt.parseTEnt 24") >> undefined -- TODO

       | entType == Constants.teDebugTrail -> do
           io (print "CLTEnt.parseTEnt 25") >> undefined -- TODO

       | entType == Constants.tePlainExplosion -> do
           io (print "CLTEnt.parseTEnt 26") >> undefined -- TODO

       | entType == Constants.teFlashlight -> do
           io (print "CLTEnt.parseTEnt 27") >> undefined -- TODO

       | entType == Constants.teForceWall -> do
           io (print "CLTEnt.parseTEnt 28") >> undefined -- TODO

       | entType == Constants.teHeatBeam -> do
           io (print "CLTEnt.parseTEnt 29") >> undefined -- TODO

       | entType == Constants.teMonsterHeatBeam -> do
           io (print "CLTEnt.parseTEnt 30") >> undefined -- TODO

       | entType == Constants.teHeatBeamSparks -> do
           io (print "CLTEnt.parseTEnt 31") >> undefined -- TODO

       | entType == Constants.teHeatBeamSteam -> do
           io (print "CLTEnt.parseTEnt 32") >> undefined -- TODO

       | entType == Constants.teSteam -> do
           io (print "CLTEnt.parseTEnt 33") >> undefined -- TODO

       | entType == Constants.teBubbleTrail2 -> do
           io (print "CLTEnt.parseTEnt 34") >> undefined -- TODO

       | entType == Constants.teMoreBlood -> do
           io (print "CLTEnt.parseTEnt 35") >> undefined -- TODO

       | entType == Constants.teChainFistSmoke -> do
           io (print "CLTEnt.parseTEnt 36") >> undefined -- TODO

       | entType == Constants.teElectricSparks -> do
           io (print "CLTEnt.parseTEnt 37") >> undefined -- TODO

       | entType == Constants.teTrackerExplosion -> do
           io (print "CLTEnt.parseTEnt 38") >> undefined -- TODO

       | any (== entType) [Constants.teTeleportEffect, Constants.teDBallGoal] -> do
           io (print "CLTEnt.parseTEnt 39") >> undefined -- TODO

       | entType == Constants.teWidowBeamOut -> do
           io (print "CLTEnt.parseTEnt 40") >> undefined -- TODO

       | entType == Constants.teNukeBlast -> do
           io (print "CLTEnt.parseTEnt 41") >> undefined -- TODO

       | entType == Constants.teWidowSplash -> do
           io (print "CLTEnt.parseTEnt 42") >> undefined -- TODO

       | otherwise -> do
           Com.comError Constants.errDrop "CL_ParseTEnt: bad type"

allocExplosion :: Quake (IORef ExplosionT)
allocExplosion = do
    explosions <- use $ clTEntGlobals.clteExplosions
    freeExRef <- findFreeSlot explosions 0 Constants.maxExplosions

    case freeExRef of
      Just ref -> do
        io $ writeIORef ref newExplosionT
        return ref
      Nothing -> do
        -- find the oldest explosion
        time <- use $ globals.cl.csTime
        exRef <- findOldestExplosion explosions (fromIntegral time) 0 0 Constants.maxExplosions
        io $ writeIORef exRef newExplosionT
        return exRef

  where findFreeSlot :: V.Vector (IORef ExplosionT) -> Int -> Int -> Quake (Maybe (IORef ExplosionT))
        findFreeSlot explosions idx maxIdx
          | idx >= maxIdx = return Nothing
          | otherwise = do
              ex <- io $ readIORef (explosions V.! idx)
              if (ex^.eType) == exFree
                then return (Just (explosions V.! idx))
                else findFreeSlot explosions (idx + 1) maxIdx

        findOldestExplosion :: V.Vector (IORef ExplosionT) -> Float -> Int -> Int -> Int -> Quake (IORef ExplosionT)
        findOldestExplosion explosions time oldestIdx idx maxIdx
          | idx >= maxIdx = return (explosions V.! oldestIdx)
          | otherwise = do
              ex <- io $ readIORef (explosions V.! idx)
              if (ex^.eStart) < time
                then findOldestExplosion explosions (ex^.eStart) idx (idx + 1) maxIdx
                else findOldestExplosion explosions time oldestIdx (idx + 1) maxIdx
