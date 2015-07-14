{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Client.CLTEnt where

import Control.Lens (zoom, (.=), use, (^.), ix)
import Control.Monad (void, when)
import Data.Bits (shiftR, (.|.))
import Data.Maybe (isNothing, fromJust)
import Linear (V3(..), _x, _y, _z, norm, normalize)
import qualified Data.Vector as V

import Quake
import QuakeState
import qualified Constants
import {-# SOURCE #-} qualified Client.V as ClientV
import qualified Util.Lib as Lib

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

clearTEnts :: Quake ()
clearTEnts = do
    zoom clTEntGlobals $ do
      clteExplosions  .= V.replicate Constants.maxExplosions newExplosionT
      clteBeams       .= V.replicate Constants.maxBeams newBeamT
      cltePlayerBeams .= V.replicate Constants.maxBeams newBeamT
      clteLasers      .= V.replicate Constants.maxLasers newLaserT
      clteSustains    .= V.replicate Constants.maxSustains newCLSustainT

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
                      let ent = newEntityT { _eOrigin = b^.bEnd
                                           , _eModel = b^.bModel
                                           , _enFlags = Constants.rfFullBright
                                           , _eAngles = V3 pitch yaw (fromIntegral $ r `mod` 360)
                                           }

                      ClientV.addEntity ent
                    else do
                      constructBeams b org len modLightning pitch yaw d' dist' modelLength
                      addBeam beams cl' (idx + 1) maxIdx

        constructBeams :: BeamT -> V3 Float -> Float -> Maybe ModelReference -> Float -> Float -> Float -> V3 Float -> Float -> Quake ()
        constructBeams b org len modLightning pitch yaw d dist modelLength
          | d <= 0 = return ()
          | otherwise = do
              let ent = newEntityT { _eOrigin = org
                                   , _eModel = b^.bModel
                                   }

              r <- Lib.rand

              if (b^.bModel) == modLightning
                then
                  ClientV.addEntity ent { _enFlags = Constants.rfFullBright
                                        , _eAngles = V3 (-pitch) (yaw + 180) (fromIntegral $ r `mod` 360)
                                        }
                else
                  ClientV.addEntity ent { _eAngles = V3 pitch yaw (fromIntegral $ r `mod` 360)
                                        }

              let org' = org + fmap (* len) dist
              constructBeams b org' len modLightning pitch yaw (d - modelLength) dist modelLength

addPlayerBeams :: Quake ()
addPlayerBeams = do
    io (putStrLn "CLTEnt.addPlayerBeams") >> undefined -- TODO

addExplosions :: Quake ()
addExplosions = do
    explosions <- use $ clTEntGlobals.clteExplosions
    cl' <- use $ globals.cl
    addExplosion explosions cl' 0 Constants.maxExplosions

  where addExplosion :: V.Vector ExplosionT -> ClientStateT -> Int -> Int -> Quake ()
        addExplosion explosions cl' idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              let ex = explosions V.! idx

              if (ex^.eType) == exFree
                then
                  addExplosion explosions cl' (idx + 1) maxIdx
                else do
                  let frac = (fromIntegral (cl'^.csTime) - (ex^.eStart)) / 100
                      f = truncate frac :: Int

                  maybeEnt <- if | (ex^.eType) == exMFlash -> do
                                     return $ if f >= (ex^.eFrames) - 1
                                                then Nothing
                                                else Just (ex^.eEnt)
                 
                                 | (ex^.eType) == exMisc -> do
                                     return $ if f >= (ex^.eFrames) - 1
                                                then Nothing
                                                else Just $ (ex^.eEnt) { _eAlpha = 1 - frac / fromIntegral ((ex^.eFrames) - 1) }
                 
                                 | (ex^.eType) == exFlash -> do
                                     return $ if f >= 1
                                                then Nothing
                                                else Just $ (ex^.eEnt) { _eAlpha = 1 }
                 
                                 | (ex^.eType) == exPoly -> do
                                     return $ if f >= (ex^.eFrames) - 1
                                                then Nothing
                                                else let alpha = (16.0 - fromIntegral f) / 16
                                                         skinNum = if f < 10
                                                                     then if (f `shiftR` 1) < 0
                                                                            then 0
                                                                            else f `shiftR` 1
                                                                     else if f < 13
                                                                            then 5
                                                                            else 6
                                                         flags = if f < 10
                                                                   then (ex^.eEnt.enFlags)
                                                                   else (ex^.eEnt.enFlags) .|. Constants.rfTranslucent
                                                     in Just $ (ex^.eEnt) { _eAlpha = alpha
                                                                          , _eSkinNum = skinNum
                                                                          , _enFlags = flags
                                                                          }
                 
                                 | (ex^.eType) == exPoly2 -> do
                                     return $ if f >= (ex^.eFrames) - 1
                                                then Nothing
                                                else let alpha = (5.0 - fromIntegral f) / 5.0
                                                         flags = (ex^.eEnt.enFlags) .|. Constants.rfTranslucent
                                                     in Just $ (ex^.eEnt) { _eAlpha = alpha
                                                                          , _eSkinNum = 0
                                                                          , _enFlags = flags
                                                                          }

                                 | otherwise -> return Nothing -- shouldn't happend tbh

                         
                  case maybeEnt of
                    Nothing -> do
                      clTEntGlobals.clteExplosions.ix idx.eType .= exFree
                      addExplosion explosions cl' (idx + 1) maxIdx
                    Just ent -> do
                      when ((ex^.eLight) /= 0) $
                        ClientV.addLight (ent^.eOrigin)
                                         ((ex^.eLight) * (ent^.eAlpha))
                                         (ex^.eLightColor._x)
                                         (ex^.eLightColor._y)
                                         (ex^.eLightColor._z)

                      let f' = if f < 0 then 0 else f

                      ClientV.addEntity ent { _eOldOrigin = ent^.eOrigin
                                            , _eFrame = (ex^.eBaseFrame) + f' + 1
                                            , _eOldFrame = (ex^.eBaseFrame) + f'
                                            , _eBackLerp = 1 - (cl'^.csLerpFrac)
                                            }

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
