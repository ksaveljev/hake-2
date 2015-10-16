{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Client.CLTEnt where

import Control.Lens (zoom, (.=), use, (^.), ix, preuse)
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
           pos <- MSG.readPos (globals.netMessage)
           dir <- MSG.readDir (globals.netMessage)

           if entType == Constants.teGunshot
             then CLFX.particleEffect pos dir 0x00 40
             else CLFX.particleEffect pos dir 0xE0 6

           when (entType /= Constants.teSparks) $ do
             smokeAndFlash pos

             -- impact sound
             r <- Lib.rand
             let cnt = r .&. 15

             case cnt of
               1 -> do
                 sfxRef <- use $ clTEntGlobals.clteSfxRic1
                 S.startSound (Just pos) worldRef 0 sfxRef 1 Constants.attnNorm 0
               2 -> do
                 sfxRef <- use $ clTEntGlobals.clteSfxRic2
                 S.startSound (Just pos) worldRef 0 sfxRef 1 Constants.attnNorm 0
               3 -> do
                 sfxRef <- use $ clTEntGlobals.clteSfxRic3
                 S.startSound (Just pos) worldRef 0 sfxRef 1 Constants.attnNorm 0
               _ ->
                 return ()

       | any (== entType) [Constants.teScreenSparks, Constants.teShieldSparks] -> do
           pos <- MSG.readPos (globals.netMessage)
           dir <- MSG.readDir (globals.netMessage)

           if entType == Constants.teScreenSparks
             then CLFX.particleEffect pos dir 0xD0 40
             else CLFX.particleEffect pos dir 0xB0 40

           -- FIXME: replace or remove this sound
           sfxRef <- use $ clTEntGlobals.clteSfxLashIt
           S.startSound (Just pos) worldRef 0 sfxRef 1 Constants.attnNorm 0

         -- bullet hitting wall
       | entType == Constants.teShotgun -> do
           pos <- MSG.readPos (globals.netMessage)
           dir <- MSG.readDir (globals.netMessage)

           CLFX.particleEffect pos dir 0 20

           smokeAndFlash pos

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
             S.startSound (Just pos) worldRef 0 sfxRef 1 Constants.attnStatic 0

       | entType == Constants.teLaserSparks -> do
           cnt <- MSG.readByte (globals.netMessage)
           pos <- MSG.readPos (globals.netMessage)
           dir <- MSG.readDir (globals.netMessage)
           color <- MSG.readByte (globals.netMessage)
           CLFX.particleEffect2 pos dir color cnt

       | entType == Constants.teBlueHyperblaster -> do
           pos <- MSG.readPos (globals.netMessage)
           dir <- MSG.readPos (globals.netMessage) -- yes jake2 and original source has readPos here
           CLFX.blasterParticles pos dir

         -- blaster hitting wall
       | entType == Constants.teBlaster -> do
           pos <- MSG.readPos (globals.netMessage)
           dir <- MSG.readDir (globals.netMessage)
           CLFX.blasterParticles pos dir

           exRef <- allocExplosion
           model <- use $ clTEntGlobals.clteModExplode

           let v = if | (dir^._x) /= 0 -> (atan2 (dir^._y) (dir^._x)) / pi * 180
                      | (dir^._y) > 0 -> 90
                      | (dir^._y) < 0 -> 270
                      | otherwise -> 0

               ent = newEntityT { _enFlags = Constants.rfFullBright .|. Constants.rfTranslucent
                                , _eAngles = V3 ((acos (dir^._z)) / pi * 180) v 0
                                , _eModel  = model
                                , _eOrigin = pos
                                }

           entRef <- io $ newIORef ent
           serverTime <- use $ globals.cl.csFrame.fServerTime

           let ex = newExplosionT { _eType       = exMisc
                                  , _eEnt        = entRef
                                  , _eFrames     = 4
                                  , _eLight      = 150
                                  , _eLightColor = V3 1.0 1.0 0.0
                                  , _eStart      = fromIntegral (serverTime  - 100)
                                  }

           io $ writeIORef exRef ex

           sfxRef <- use $ clTEntGlobals.clteSfxLashIt
           S.startSound (Just pos) worldRef 0 sfxRef 1 Constants.attnNorm 0

         -- railgun effect
       | entType == Constants.teRailTrail -> do
           pos <- MSG.readPos (globals.netMessage)
           pos2 <- MSG.readPos (globals.netMessage)
           CLFX.railTrail pos pos2
           sfxRef <- use $ clTEntGlobals.clteSfxRailg
           S.startSound (Just pos2) worldRef 0 sfxRef 1 Constants.attnNorm 0

       | any (== entType) [Constants.teExplosion2, Constants.teGrenadeExplosion, Constants.teGrenadeExplosionWater] -> do
           pos <- MSG.readPos (globals.netMessage)

           exRef <- allocExplosion

           r <- liftM (fromIntegral . (`mod` 360)) Lib.rand
           model <- use $ clTEntGlobals.clteModExplo4

           let ent = newEntityT { _enFlags = Constants.rfFullBright
                                , _eAngles = V3 0 r 0
                                , _eModel  = model
                                , _eOrigin = pos
                                }

           entRef <- io $ newIORef ent
           serverTime <- use $ globals.cl.csFrame.fServerTime

           let ex = ExplosionT { _eType       = exPoly
                               , _eEnt        = entRef
                               , _eFrames     = 19
                               , _eLight      = 350
                               , _eLightColor = V3 1.0 0.5 0.5
                               , _eStart      = fromIntegral (serverTime  - 100)
                               , _eBaseFrame  = 30
                               }

           io $ writeIORef exRef ex

           CLFX.explosionParticles pos

           sfxRef <- if entType == Constants.teGrenadeExplosionWater
                       then use $ clTEntGlobals.clteSfxWatrExp
                       else use $ clTEntGlobals.clteSfxGrenExp

           S.startSound (Just pos) worldRef 0 sfxRef 1 Constants.attnNorm 0

       | entType == Constants.tePlasmaExplosion -> do
           pos <- MSG.readPos (globals.netMessage)

           exRef <- allocExplosion

           r <- liftM (fromIntegral . (`mod` 360)) Lib.rand
           model <- use $ clTEntGlobals.clteModExplo4

           let ent = newEntityT { _enFlags = Constants.rfFullBright
                                , _eAngles = V3 0 r 0
                                , _eModel  = model
                                , _eOrigin = pos
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

           CLFX.explosionParticles pos

           sfxRef <- use $ clTEntGlobals.clteSfxRockExp
           S.startSound (Just pos) worldRef 0 sfxRef 1 Constants.attnNorm 0

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
                                , _eOrigin = pos
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

           S.startSound (Just pos) worldRef 0 sfxRef 1 Constants.attnNorm 0

       | entType == Constants.teBfgExplosion -> do
           pos <- MSG.readPos (globals.netMessage)

           exRef <- allocExplosion

           model <- use $ clTEntGlobals.clteModBfgExplo

           let ent = newEntityT { _enFlags = Constants.rfFullBright .|. Constants.rfTranslucent
                                , _eModel  = model
                                , _eOrigin = pos
                                , _eAlpha = 0.3
                                }

           entRef <- io $ newIORef ent
           serverTime <- use $ globals.cl.csFrame.fServerTime

           let ex = newExplosionT { _eType       = exPoly
                                  , _eEnt        = entRef
                                  , _eFrames     = 4
                                  , _eLight      = 350
                                  , _eLightColor = V3 0.0 1.0 0.0
                                  , _eStart      = fromIntegral (serverTime  - 100)
                                  }

           io $ writeIORef exRef ex

       | entType == Constants.teBfgBigExplosion -> do
           pos <- MSG.readPos (globals.netMessage)
           CLFX.bfgExplosionParticles pos

       | entType == Constants.teBfgLaser ->
           parseLaser 0xD0D1D2D3

       | entType == Constants.teBubbleTrail -> do
           pos <- MSG.readPos (globals.netMessage)
           pos2 <- MSG.readPos (globals.netMessage)
           CLFX.bubbleTrail pos pos2

       | any (== entType) [Constants.teParasiteAttack, Constants.teMedicCableAttack] -> do
           Just modParasiteSegment <- use $ clTEntGlobals.clteModParasiteSegment
           void $ parseBeam modParasiteSegment

         -- boss teleporting to station
       | entType == Constants.teBossTPort -> do
           pos <- MSG.readPos (globals.netMessage)
           CLFX.bigTeleportParticles pos
           soundIdx <- S.registerSound "misc/bigtele.wav"
           S.startSound (Just pos) worldRef 0 soundIdx 1 Constants.attnNone 0

       | entType == Constants.teGrappleCable -> do
           Just modGrappleCable <- use $ clTEntGlobals.clteModGrappleCable
           void $ parseBeam2 modGrappleCable

       | entType == Constants.teWeldingSparks -> do
           cnt <- MSG.readByte (globals.netMessage)
           pos <- MSG.readPos (globals.netMessage)
           dir <- MSG.readDir (globals.netMessage)
           color <- MSG.readByte (globals.netMessage)

           CLFX.particleEffect2 pos dir color cnt

           exRef <- allocExplosion

           flashModel <- use $ clTEntGlobals.clteModFlash

           let ent = newEntityT { _enFlags = Constants.rfBeam
                                , _eModel  = flashModel
                                , _eOrigin = pos
                                }

           entRef <- io $ newIORef ent
           serverTime <- use $ globals.cl.csFrame.fServerTime
           r <- Lib.rand

           let ex = newExplosionT { _eType       = exFlash
                                  , _eEnt        = entRef
                                  , _eStart      = fromIntegral serverTime - 0.1
                                  , _eLight      = 100 + fromIntegral (r `mod` 75)
                                  , _eLightColor = V3 1.0 1.0 0.3
                                  , _eFrames     = 2
                                  }

           io $ writeIORef exRef ex

       | entType == Constants.teGreenBlood -> do
           pos <- MSG.readPos (globals.netMessage)
           dir <- MSG.readDir (globals.netMessage)
           CLFX.particleEffect2 pos dir 0xDF 30

       | entType == Constants.teTunnelSparks -> do
           cnt <- MSG.readByte (globals.netMessage)
           pos <- MSG.readPos (globals.netMessage)
           dir <- MSG.readDir (globals.netMessage)
           color <- MSG.readByte (globals.netMessage)
           CLFX.particleEffect3 pos dir color cnt

       | any (== entType) [Constants.teBlaster2, Constants.teFlechette] -> do
           pos <- MSG.readPos (globals.netMessage)
           dir <- MSG.readDir (globals.netMessage)

           if entType == Constants.teBlaster2
             then CLNewFX.blasterParticles2 pos dir 0xD0
             else CLNewFX.blasterParticles2 pos dir 0x6F

           exRef <- allocExplosion

           explodeModel <- use $ clTEntGlobals.clteModExplode

           let a = (acos (dir^._z)) / pi * 180
               b = if | (dir^._x) /= 0 -> (atan2 (dir^._y) (dir^._x)) / pi * 180
                      | (dir^._y) > 0 -> 90
                      | (dir^._y) < 0 -> 270
                      | otherwise -> 0
               ent = newEntityT { _enFlags  = Constants.rfFullBright .|. Constants.rfTranslucent
                                , _eSkinNum = if entType == Constants.teBlaster2 then 1 else 2
                                , _eModel   = explodeModel
                                , _eOrigin  = pos
                                , _eAngles  = V3 a b 0
                                }

           entRef <- io $ newIORef ent
           serverTime <- use $ globals.cl.csFrame.fServerTime

           let ex = newExplosionT { _eType       = exMisc
                                  , _eEnt        = entRef
                                  , _eStart      = fromIntegral (serverTime - 100)
                                  , _eLight      = 150
                                  , _eLightColor = if entType == Constants.teBlaster2 then V3 0 1 0 else V3 0.19 0.41 0.75
                                  , _eFrames     = 4
                                  }

           io $ writeIORef exRef ex

           sfxLashIt <- use $ clTEntGlobals.clteSfxLashIt

           S.startSound (Just pos) worldRef 0 sfxLashIt 1 Constants.attnNorm 0

       | entType == Constants.teLightning -> do
           Just modLightning <- use $ clTEntGlobals.clteModLightning
           ent <- parseLightning modLightning
           sfxLightning <- use $ clTEntGlobals.clteSfxLightning
           S.startSound Nothing (newEdictReference ent) Constants.chanWeapon sfxLightning 1 Constants.attnNorm 0

       | entType == Constants.teDebugTrail -> do
           pos <- MSG.readPos (globals.netMessage)
           pos2 <- MSG.readPos (globals.netMessage)
           CLNewFX.debugTrail pos pos2

       | entType == Constants.tePlainExplosion -> do
           pos <- MSG.readPos (globals.netMessage)

           exRef <- allocExplosion

           r <- Lib.rand
           f <- Lib.randomF
           explo4Model <- use $ clTEntGlobals.clteModExplo4

           let ent = newEntityT { _enFlags  = Constants.rfFullBright
                                , _eModel   = explo4Model
                                , _eOrigin  = pos
                                , _eAngles  = V3 0 (fromIntegral $ r `mod` 360) 0
                                }

           entRef <- io $ newIORef ent
           serverTime <- use $ globals.cl.csFrame.fServerTime

           let ex = newExplosionT { _eType       = exPoly
                                  , _eEnt        = entRef
                                  , _eStart      = fromIntegral (serverTime - 100)
                                  , _eLight      = 350
                                  , _eLightColor = V3 1.0 0.5 0.5
                                  , _eFrames     = 15
                                  , _eBaseFrame  = if f < 0.5 then 15 else 0
                                  }

           io $ writeIORef exRef ex

           sfx <- if entType == Constants.teRocketExplosionWater
                    then use $ clTEntGlobals.clteSfxWatrExp
                    else use $ clTEntGlobals.clteSfxRockExp

           S.startSound (Just pos) worldRef 0 sfx 1 Constants.attnNorm 0

       | entType == Constants.teFlashlight -> do
           pos <- MSG.readPos (globals.netMessage)
           ent <- MSG.readShort (globals.netMessage)
           CLNewFX.flashlight ent pos

       | entType == Constants.teForceWall -> do
           pos <- MSG.readPos (globals.netMessage)
           pos2 <- MSG.readPos (globals.netMessage)
           color <- MSG.readByte (globals.netMessage)
           CLNewFX.forceWall pos pos2 color

       | entType == Constants.teHeatBeam -> do
           Just modHeatBeam <- use $ clTEntGlobals.clteModHeatBeam
           void $ parsePlayerBeam modHeatBeam

       | entType == Constants.teMonsterHeatBeam -> do
           Just modMonsterHeatBeam <- use $ clTEntGlobals.clteModMonsterHeatBeam
           void $ parsePlayerBeam modMonsterHeatBeam

       | entType == Constants.teHeatBeamSparks -> do
           pos <- MSG.readPos (globals.netMessage)
           dir <- MSG.readDir (globals.netMessage)
           let cnt = 50
               r = 8
               magnitude = 60
               color = r .&. 0xFF

           CLNewFX.particleSteamEffect pos dir color cnt magnitude

           sfxLashIt <- use $ clTEntGlobals.clteSfxLashIt
           S.startSound (Just pos) worldRef 0 sfxLashIt 1 Constants.attnNorm 0

       | entType == Constants.teHeatBeamSteam -> do
           pos <- MSG.readPos (globals.netMessage)
           dir <- MSG.readDir (globals.netMessage)

           let cnt = 20
               color = 0xE0
               magnitude = 60

           CLNewFX.particleSteamEffect pos dir color cnt magnitude

           sfxLashIt <- use $ clTEntGlobals.clteSfxLashIt
           S.startSound (Just pos) worldRef 0 sfxLashIt 1 Constants.attnNorm 0

       | entType == Constants.teSteam -> do
           parseSteam

       | entType == Constants.teBubbleTrail2 -> do
           pos <- MSG.readPos (globals.netMessage)
           pos2 <- MSG.readPos (globals.netMessage)

           CLNewFX.bubbleTrail2 pos pos2 8

           sfxLashIt <- use $ clTEntGlobals.clteSfxLashIt
           S.startSound (Just pos) worldRef 0 sfxLashIt 1 Constants.attnNorm 0

       | entType == Constants.teMoreBlood -> do
           pos <- MSG.readPos (globals.netMessage)
           dir <- MSG.readDir (globals.netMessage)
           CLFX.particleEffect pos dir 0xE8 250

       | entType == Constants.teChainFistSmoke -> do
           pos <- MSG.readPos (globals.netMessage)
           CLNewFX.particleSmokeEffect pos (V3 0 0 1) 0 20 20

       | entType == Constants.teElectricSparks -> do
           pos <- MSG.readPos (globals.netMessage)
           dir <- MSG.readDir (globals.netMessage)
           CLFX.particleEffect pos dir 0x75 40

           sfxLashIt <- use $ clTEntGlobals.clteSfxLashIt
           S.startSound (Just pos) worldRef 0 sfxLashIt 1 Constants.attnNorm 0

       | entType == Constants.teTrackerExplosion -> do
           pos <- MSG.readPos (globals.netMessage)

           CLNewFX.colorFlash pos 0 150 (-1) (-1) (-1)
           CLNewFX.colorExplosionParticles pos 0 1

           sfxDisrExp <- use $ clTEntGlobals.clteSfxDisrExp
           S.startSound (Just pos) worldRef 0 sfxDisrExp 1 Constants.attnNorm 0

       | any (== entType) [Constants.teTeleportEffect, Constants.teDBallGoal] -> do
           pos <- MSG.readPos (globals.netMessage)
           CLFX.teleportParticles pos

       | entType == Constants.teWidowBeamOut -> do
           parseWidow

       | entType == Constants.teNukeBlast -> do
           parseNuke

       | entType == Constants.teWidowSplash -> do
           pos <- MSG.readPos (globals.netMessage)
           CLNewFX.widowSplash pos

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

smokeAndFlash :: V3 Float -> Quake ()
smokeAndFlash origin = do
    setFirstEx
    setSecondEx

  where setFirstEx :: Quake ()
        setFirstEx = do
          exRef <- allocExplosion

          smokeModel <- use $ clTEntGlobals.clteModSmoke

          let ent = newEntityT { _enFlags = Constants.rfTranslucent
                               , _eModel  = smokeModel
                               , _eOrigin = origin
                               }

          entRef <- io $ newIORef ent
          serverTime <- use $ globals.cl.csFrame.fServerTime

          let ex = newExplosionT { _eType       = exMisc
                                 , _eEnt        = entRef
                                 , _eFrames     = 4
                                 , _eStart      = fromIntegral (serverTime  - 100)
                                 }

          io $ writeIORef exRef ex

        setSecondEx :: Quake ()
        setSecondEx = do
          exRef <- allocExplosion

          flashModel <- use $ clTEntGlobals.clteModFlash

          let ent = newEntityT { _enFlags = Constants.rfFullBright
                               , _eModel  = flashModel
                               , _eOrigin = origin
                               }

          entRef <- io $ newIORef ent
          serverTime <- use $ globals.cl.csFrame.fServerTime

          let ex = newExplosionT { _eType       = exFlash
                                 , _eEnt        = entRef
                                 , _eFrames     = 2
                                 , _eStart      = fromIntegral (serverTime  - 100)
                                 }

          io $ writeIORef exRef ex

parseLaser :: Int -> Quake ()
parseLaser colors = do
    start <- MSG.readPos (globals.netMessage)
    end <- MSG.readPos (globals.netMessage)

    time <- use $ globals.cl.csTime
    updateLaser time start end 0 Constants.maxLasers
  
  where updateLaser :: Int -> V3 Float -> V3 Float -> Int -> Int -> Quake ()
        updateLaser time start end idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              Just laser <- preuse $ clTEntGlobals.clteLasers.ix idx

              if (laser^.lEndTime) < time
                then do
                  r <- Lib.rand
                  io $ modifyIORef' (laser^.lEnt) (\v -> v { _enFlags = Constants.rfTranslucent .|. Constants.rfBeam
                                                           , _eOrigin = start
                                                           , _eOldOrigin = end
                                                           , _eAlpha = 0.3
                                                           , _eSkinNum = (colors `shiftR` fromIntegral ((r `mod` 4) * 8)) .&. 0xFF
                                                           , _eModel = Nothing
                                                           , _eFrame = 4
                                                           })

                  clTEntGlobals.clteLasers.ix idx.lEndTime .= time + 100

                else
                  updateLaser time start end (idx + 1) maxIdx

parseBeam :: IORef ModelT -> Quake Int
parseBeam _ = do
    io (putStrLn "CLTEnt.parseBeam") >> undefined -- TODO

parseBeam2 :: IORef ModelT -> Quake Int
parseBeam2 _ = do
    io (putStrLn "CLTEnt.parseBeam2") >> undefined -- TODO

parseLightning :: IORef ModelT -> Quake Int
parseLightning _ = do
    io (putStrLn "CLTEnt.parseLightning") >> undefined -- TODO

parsePlayerBeam :: IORef ModelT -> Quake Int
parsePlayerBeam _ = do
    io (putStrLn "CLTEnt.parsePlayerBeam") >> undefined -- TODO

parseSteam :: Quake ()
parseSteam = do
    io (putStrLn "CLTEnt.parseSteam") >> undefined -- TODO

parseWidow :: Quake ()
parseWidow = do
    io (putStrLn "CLTEnt.praseWidow") >> undefined -- TODO

parseNuke :: Quake ()
parseNuke = do
    io (putStrLn "CLTEnt.parseNuke") >> undefined -- TODO
