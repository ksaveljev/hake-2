module Client.CLNewFX where

import Control.Lens (use, (^.), (.=), (&), (-~), (+~))
import Control.Monad (unless)
import Data.Bits ((.&.))
import Data.IORef (IORef, readIORef, modifyIORef')
import Linear (V3(..), normalize, norm, _z)

import Quake
import QuakeState
import qualified Client.CLFX as CLFX
import qualified Util.Lib as Lib
import qualified Util.Math3D as Math3D

monsterPlasmaShell :: V3 Float -> Quake ()
monsterPlasmaShell origin = do
    freeParticles <- use $ clientGlobals.cgFreeParticles
    addMonsterPlasmaShell freeParticles 0 40

  where addMonsterPlasmaShell :: Maybe (IORef CParticleT) -> Int -> Int -> Quake ()
        addMonsterPlasmaShell Nothing _ _ = return ()
        addMonsterPlasmaShell (Just pRef) idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              p <- io $ readIORef pRef
              clientGlobals.cgFreeParticles .= (p^.cpNext)
              activeParticles <- use $ clientGlobals.cgActiveParticles
              clientGlobals.cgActiveParticles .= Just pRef

              time <- use $ globals.cl.csTime
              o1 <- Lib.crandom
              o2 <- Lib.crandom
              o3 <- Lib.crandom

              io $ modifyIORef' pRef (\v -> v { _cpNext = activeParticles
                                              , _cpAccel = V3 0 0 0
                                              , _cpTime = fromIntegral time
                                              , _cpAlpha = 1.0
                                              , _cpAlphaVel = CLFX.instantParticle
                                              , _cpColor = 0xE0
                                              , _cpOrg = origin + fmap (* 10) (normalize (V3 o1 o2 o3))
                                              })

              addMonsterPlasmaShell (p^.cpNext) (idx + 1) maxIdx

heatBeam :: V3 Float -> V3 Float -> Quake ()
heatBeam start forward = do
    let end = start + fmap (* 4096) forward
        vec = end - start
        len = norm vec
        vec' = normalize vec

    -- FIXME - pmm - these might end up using old values?
    --               MakeNormalVectors (vec, right, up)
    right <- use $ globals.cl.csVRight
    up <- use $ globals.cl.csVUp

    -- TODO: if (Globals.vidref_val == Defines.VIDREF_GL) { // GL mode
    let move = start + fmap (* (-0.5)) right + fmap (* (-0.5)) up
    -- otherwise assume SOFT


    time <- use $ globals.cl.csTime
    let ltime = fromIntegral time / 1000.0
        startPt = truncate (ltime * 96) `mod` 32 :: Int
        move' = move + fmap (* (fromIntegral startPt)) vec'
        vec'' = fmap (* 32) vec'
        rstep = pi / 10.0

    addRings (fromIntegral startPt) vec'' move' len

  where addRings :: Float -> V3 Float -> V3 Float -> Float -> Quake ()
        addRings i vec move len
          | i >= len = return ()
          | i > 32 * 5 = return () -- don't bother after the 5th ring
          | otherwise = do
              freeParticles <- use $ clientGlobals.cgFreeParticles
              done <- addRingParticles freeParticles i move 0 (pi * 2.0)
              unless done $
                addRings (i + 32) vec (move + vec) len

        addRingParticles :: Maybe (IORef CParticleT) -> Float -> V3 Float -> Float -> Float -> Quake Bool
        addRingParticles Nothing _ _ _ _ = return True
        addRingParticles (Just pRef) i move rot maxRot
          | rot >= maxRot = return False
          | otherwise = do
              p <- io $ readIORef pRef
              clientGlobals.cgFreeParticles .= (p^.cpNext)
              activeParticles <- use $ clientGlobals.cgActiveParticles
              clientGlobals.cgActiveParticles .= Just pRef

              time <- use $ globals.cl.csTime
              right <- use $ globals.cl.csVRight
              up <- use $ globals.cl.csVUp
              r <- Lib.rand

              let c = (cos rot) * 0.5
                  s = (sin rot) * 0.5
                  dir = if i < 10
                          then fmap (* (c * (i / 10.0))) right + fmap (* (s * (i / 10.0))) up
                          else fmap (* c) right + fmap (* s) up

              io $ modifyIORef' pRef (\v -> v { _cpNext = activeParticles
                                              , _cpTime = fromIntegral time
                                              , _cpAccel = V3 0 0 0
                                              , _cpAlpha = 0.5
                                              , _cpAlphaVel = -1000.0
                                              , _cpColor = 223 - fromIntegral (r .&. 7)
                                              , _cpOrg = move + fmap (* 3) dir
                                              , _cpVel = V3 0 0 0
                                              })

              addRingParticles (p^.cpNext) i move (rot + pi / 10.0) maxRot

tagTrail :: V3 Float -> V3 Float -> Float -> Quake ()
tagTrail start end color = do
    let move = start
        vec = end - start
        len = norm vec
        vec' = fmap (* 5) (normalize vec)

    freeParticles <- use $ clientGlobals.cgFreeParticles
    addTagTrail freeParticles vec' move len

  where addTagTrail :: Maybe (IORef CParticleT) -> V3 Float -> V3 Float -> Float -> Quake ()
        addTagTrail Nothing _ _ _ = return ()
        addTagTrail (Just pRef) vec move len
          | len < 0 = return ()
          | otherwise = do
              p <- io $ readIORef pRef
              clientGlobals.cgFreeParticles .= (p^.cpNext)
              activeParticles <- use $ clientGlobals.cgActiveParticles
              clientGlobals.cgActiveParticles .= Just pRef

              time <- use $ globals.cl.csTime
              f <- Lib.randomF
              o1 <- Lib.crandom
              o2 <- Lib.crandom
              o3 <- Lib.crandom
              v1 <- Lib.crandom
              v2 <- Lib.crandom
              v3 <- Lib.crandom

              io $ modifyIORef' pRef (\v -> v { _cpNext = activeParticles
                                              , _cpAccel = V3 0 0 0
                                              , _cpTime = fromIntegral time
                                              , _cpAlpha = 1.0
                                              , _cpAlphaVel = (-1.0) / (0.8 + f * 0.2)
                                              , _cpColor = color
                                              , _cpOrg = move + fmap (* 16) (V3 o1 o2 o3)
                                              , _cpVel = fmap (* 5) (V3 v1 v2 v3)
                                              })

              addTagTrail (p^.cpNext) vec (move + vec) (len - 5)

blasterTrail2 :: V3 Float -> V3 Float -> Quake ()
blasterTrail2 start end = do
    let move = start
        vec = end - start
        len = norm vec
        vec' = fmap (* 5) (normalize vec)

    freeParticles <- use $ clientGlobals.cgFreeParticles
    addBlasterTrail2 freeParticles vec' move len

  where addBlasterTrail2 :: Maybe (IORef CParticleT) -> V3 Float -> V3 Float -> Float -> Quake ()
        addBlasterTrail2 Nothing _ _ _ = return ()
        addBlasterTrail2 (Just pRef) vec move len
          | len <= 0 = return ()
          | otherwise = do
              p <- io $ readIORef pRef
              clientGlobals.cgFreeParticles .= (p^.cpNext)
              activeParticles <- use $ clientGlobals.cgActiveParticles
              clientGlobals.cgActiveParticles .= Just pRef

              time <- use $ globals.cl.csTime
              f <- Lib.randomF
              o1 <- Lib.crandom
              o2 <- Lib.crandom
              o3 <- Lib.crandom
              v1 <- Lib.crandom
              v2 <- Lib.crandom
              v3 <- Lib.crandom

              io $ modifyIORef' pRef (\v -> v { _cpNext = activeParticles
                                              , _cpTime = fromIntegral time
                                              , _cpAccel = V3 0 0 0
                                              , _cpAlpha = 1.0
                                              , _cpAlphaVel = (-1.0) / (0.3 + f * 0.2)
                                              , _cpColor = 0xD0
                                              , _cpOrg = move + V3 o1 o2 o3
                                              , _cpVel = fmap (* 5) (V3 v1 v2 v3)
                                              })

              addBlasterTrail2 (p^.cpNext) vec (move + vec) (len - 5)

blasterParticles2 :: V3 Float -> V3 Float -> Int -> Quake ()
blasterParticles2 org dir color = do
    freeParticles <- use $ clientGlobals.cgFreeParticles
    addBlasterParticles2 freeParticles 0 40

  where addBlasterParticles2 :: Maybe (IORef CParticleT) -> Int -> Int -> Quake ()
        addBlasterParticles2 Nothing _ _ = return ()
        addBlasterParticles2 (Just pRef) idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              p <- io $ readIORef pRef
              clientGlobals.cgFreeParticles .= (p^.cpNext)
              activeParticles <- use $ clientGlobals.cgActiveParticles
              clientGlobals.cgActiveParticles .= Just pRef

              time <- use $ globals.cl.csTime
              f <- Lib.randomF
              r <- Lib.rand
              d <- Lib.rand >>= \d -> return (fromIntegral (d .&. 15))
              o1 <- Lib.rand
              o2 <- Lib.rand
              o3 <- Lib.rand
              v1 <- Lib.crandom
              v2 <- Lib.crandom
              v3 <- Lib.crandom

              io $ modifyIORef' pRef (\v -> v { _cpNext = activeParticles
                                              , _cpTime = fromIntegral time
                                              , _cpAccel = V3 0 0 (- CLFX.particleGravity)
                                              , _cpColor = fromIntegral color + fromIntegral (r .&. 7)
                                              , _cpOrg = org + fmap (fromIntegral . (subtract 4) . (.&. 7)) (V3 o1 o2 o3) + fmap (* d) dir
                                              , _cpVel = fmap (* 30) dir + fmap (* 40) (V3 v1 v2 v3)
                                              , _cpAlpha = 1.0
                                              , _cpAlphaVel = (-1.0) / (0.5 + f * 0.3)
                                              })

              addBlasterParticles2 (p^.cpNext) (idx + 1) maxIdx

debugTrail :: V3 Float -> V3 Float -> Quake ()
debugTrail start end = do
    let move = start
        vec = end - start
        len = norm vec
        vec' = normalize vec
        (right, up) = Math3D.makeNormalVectors vec'
        vec'' = fmap (* 3) vec

    freeParticles <- use $ clientGlobals.cgFreeParticles
    addDebugTrail freeParticles vec'' move len

  where addDebugTrail :: Maybe (IORef CParticleT) -> V3 Float -> V3 Float -> Float -> Quake ()
        addDebugTrail Nothing _ _ _ = return ()
        addDebugTrail (Just pRef) vec move len
          | len <= 0 = return ()
          | otherwise = do
              p <- io $ readIORef pRef
              clientGlobals.cgFreeParticles .= (p^.cpNext)
              activeParticles <- use $ clientGlobals.cgActiveParticles
              clientGlobals.cgActiveParticles .= Just pRef

              time <- use $ globals.cl.csTime
              r <- Lib.rand

              io $ modifyIORef' pRef (\v -> v { _cpNext = activeParticles
                                              , _cpTime = fromIntegral time
                                              , _cpAccel = V3 0 0 0
                                              , _cpVel = V3 0 0 0
                                              , _cpAlpha = 1.0
                                              , _cpAlphaVel = -0.1
                                              , _cpColor = 0x74 + fromIntegral (r .&. 7)
                                              , _cpOrg = move
                                              })

              addDebugTrail (p^.cpNext) vec (move + vec) (len - 3)

flashlight :: Int -> V3 Float -> Quake ()
flashlight ent pos = do
    dlRef <- CLFX.allocDLight ent
    time <- use $ globals.cl.csTime

    io $ modifyIORef' dlRef (\v -> v { _cdlOrigin = pos
                                     , _cdlRadius = 400
                                     , _cdlMinLight = 250
                                     , _cdlDie = fromIntegral time + 100
                                     , _cdlColor = V3 1 1 1
                                     })

forceWall :: V3 Float -> V3 Float -> Int -> Quake ()
forceWall start end color = do
    let move = start
        vec = end - start
        len = norm vec
        vec' = fmap (* 4) (normalize vec)

    freeParticles <- use $ clientGlobals.cgFreeParticles
    addForceWall freeParticles vec' move len

  where addForceWall :: Maybe (IORef CParticleT) -> V3 Float -> V3 Float -> Float -> Quake ()
        addForceWall Nothing _ _ _ = return ()
        addForceWall (Just pRef) vec move len
          | len <= 0 = return ()
          | otherwise = do
              f <- Lib.randomF

              if f > 0.3
                then do
                  p <- io $ readIORef pRef
                  clientGlobals.cgFreeParticles .= (p^.cpNext)
                  activeParticles <- use $ clientGlobals.cgActiveParticles
                  clientGlobals.cgActiveParticles .= Just pRef

                  time <- use $ globals.cl.csTime
                  f' <- Lib.randomF
                  o1 <- Lib.crandom
                  o2 <- Lib.crandom
                  o3 <- Lib.crandom
                  v2 <- Lib.crandom

                  io $ modifyIORef' pRef (\v -> v { _cpNext = activeParticles
                                                  , _cpTime = fromIntegral time
                                                  , _cpAccel = V3 0 0 0
                                                  , _cpAlpha = 1.0
                                                  , _cpAlphaVel = (-1.0) / (3.0 + f' * 0.5)
                                                  , _cpColor = fromIntegral color
                                                  , _cpOrg = move + fmap (* 3) (V3 o1 o2 o3)
                                                  , _cpVel = V3 0 0 ((-40) - v2 * 10)
                                                  })

                  addForceWall (p^.cpNext) vec (move + vec) (len - 4)

                else
                  addForceWall (Just pRef) vec (move + vec) (len - 4)

particleSteamEffect :: V3 Float -> V3 Float -> Int -> Int -> Int -> Quake ()
particleSteamEffect org dir color count magnitude = do
    let (r, u) = Math3D.makeNormalVectors dir
    freeParticles <- use $ clientGlobals.cgFreeParticles
    addParticleSteamEffect freeParticles r u 0

  where addParticleSteamEffect :: Maybe (IORef CParticleT) -> V3 Float -> V3 Float -> Int -> Quake ()
        addParticleSteamEffect Nothing _ _ _ = return ()
        addParticleSteamEffect (Just pRef) r u idx
          | idx >= count = return ()
          | otherwise = do
              p <- io $ readIORef pRef
              clientGlobals.cgFreeParticles .= (p^.cpNext)
              activeParticles <- use $ clientGlobals.cgActiveParticles
              clientGlobals.cgActiveParticles .= Just pRef

              time <- use $ globals.cl.csTime
              r' <- Lib.rand
              f <- Lib.randomF
              o1 <- Lib.crandom
              o2 <- Lib.crandom
              o3 <- Lib.crandom
              d <- Lib.crandom >>= \c -> return (c * fromIntegral magnitude / 3)
              d' <- Lib.crandom >>= \c -> return (c * fromIntegral magnitude / 3)

              io $ modifyIORef' pRef (\v -> v { _cpNext = activeParticles
                                              , _cpTime = fromIntegral time
                                              , _cpColor = fromIntegral color + fromIntegral (r' .&. 7)
                                              , _cpOrg = org + fmap (* (fromIntegral magnitude * 0.1)) (V3 o1 o2 o3)
                                              , _cpVel = fmap (* fromIntegral magnitude) dir + fmap (* d) r + fmap (* d') u
                                              , _cpAccel = V3 0 0 (- CLFX.particleGravity / 2)
                                              , _cpAlpha = 1.0
                                              , _cpAlphaVel = (-1.0) / (0.5 + f * 0.3)
                                              })

              addParticleSteamEffect (p^.cpNext) r u (idx + 1)

bubbleTrail2 :: V3 Float -> V3 Float -> Float -> Quake ()
bubbleTrail2 start end dist = do
    let move = start
        vec = end - start
        len = norm vec
        vec' = fmap (* dist) (normalize vec)

    freeParticles <- use $ clientGlobals.cgFreeParticles
    addBubbleTrail2 freeParticles vec' move 0 len

  where addBubbleTrail2 :: Maybe (IORef CParticleT) -> V3 Float -> V3 Float -> Float -> Float -> Quake ()
        addBubbleTrail2 Nothing _ _ _ _ = return ()
        addBubbleTrail2 (Just pRef) vec move i len
          | i >= len = return ()
          | otherwise = do
              p <- io $ readIORef pRef
              clientGlobals.cgFreeParticles .= (p^.cpNext)
              activeParticles <- use $ clientGlobals.cgActiveParticles
              clientGlobals.cgActiveParticles .= Just pRef

              time <- use $ globals.cl.csTime
              f <- Lib.randomF
              r <- Lib.rand
              o1 <- Lib.crandom
              o2 <- Lib.crandom
              o3 <- Lib.crandom
              v1 <- Lib.crandom
              v2 <- Lib.crandom
              v3 <- Lib.crandom

              io $ modifyIORef' pRef (\v -> v { _cpNext = activeParticles
                                              , _cpTime = fromIntegral time
                                              , _cpAccel = V3 0 0 0
                                              , _cpAlpha = 1.0
                                              , _cpAlphaVel = (-1.0) / (1.0 + f * 0.1)
                                              , _cpColor = 4 + fromIntegral (r .&. 7)
                                              , _cpOrg = (move + fmap (* 2) (V3 o1 o2 o3)) & _z -~ 4
                                              , _cpVel = (fmap (* 10) (V3 v1 v2 v3)) & _z +~ 20
                                              })

              addBubbleTrail2 (p^.cpNext) vec (move + vec) (i + dist) len

particleSmokeEffect :: V3 Float -> V3 Float -> Int -> Int -> Int -> Quake ()
particleSmokeEffect org dir color count magnitude = do
    let (r, u) = Math3D.makeNormalVectors dir
    freeParticles <- use $ clientGlobals.cgFreeParticles
    addParticleSmokeEffect freeParticles r u 0

  where addParticleSmokeEffect :: Maybe (IORef CParticleT) -> V3 Float -> V3 Float -> Int -> Quake ()
        addParticleSmokeEffect Nothing _ _ _ = return ()
        addParticleSmokeEffect (Just pRef) r u idx
          | idx >= count = return ()
          | otherwise = do
              p <- io $ readIORef pRef
              clientGlobals.cgFreeParticles .= (p^.cpNext)
              activeParticles <- use $ clientGlobals.cgActiveParticles
              clientGlobals.cgActiveParticles .= Just pRef

              let magnitude' = fromIntegral magnitude

              time <- use $ globals.cl.csTime
              r' <- Lib.rand
              o1 <- Lib.crandom
              o2 <- Lib.crandom
              o3 <- Lib.crandom
              f <- Lib.randomF
              d <- Lib.crandom >>= \c -> return (c * magnitude' / 3)
              d' <- Lib.crandom >>= \c -> return (c * magnitude' / 3)

              io $ modifyIORef' pRef (\v -> v { _cpNext = activeParticles
                                              , _cpTime = fromIntegral time
                                              , _cpColor = fromIntegral color + fromIntegral (r' .&. 7)
                                              , _cpOrg = org + fmap (* (magnitude' * 0.1)) (V3 o1 o2 o3)
                                              , _cpVel = fmap (* magnitude') dir + fmap (* d) r + fmap (* d') u
                                              , _cpAccel = V3 0 0 0
                                              , _cpAlpha = 1.0
                                              , _cpAlphaVel = (-1.0) / (0.5 + f * 0.3)
                                              })

              addParticleSmokeEffect (p^.cpNext) r u (idx + 1)

colorFlash :: V3 Float -> Int -> Int -> Float -> Float -> Float -> Quake ()
colorFlash pos ent intensity r g b = do
    -- TODO: if ((Globals.vidref_val == Defines.VIDREF_SOFT)
    let (r', g', b', intensity') = if r < 0 || g < 0 || b < 0
                                     then (-r, -g, -b, -intensity)
                                     else (r, g, b, intensity)

    time <- use $ globals.cl.csTime

    dlRef <- CLFX.allocDLight ent
    io $ modifyIORef' dlRef (\v -> v { _cdlOrigin = pos
                                     , _cdlRadius = fromIntegral intensity'
                                     , _cdlMinLight = 250
                                     , _cdlDie = fromIntegral time + 100
                                     , _cdlColor = V3 r' g' b'
                                     })

colorExplosionParticles :: V3 Float -> Int -> Int -> Quake ()
colorExplosionParticles org color run = do
    freeParticles <- use $ clientGlobals.cgFreeParticles
    addColorExplosionParticles freeParticles 0 128

  where addColorExplosionParticles :: Maybe (IORef CParticleT) -> Int -> Int -> Quake ()
        addColorExplosionParticles Nothing _ _ = return ()
        addColorExplosionParticles (Just pRef) idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              p <- io $ readIORef pRef
              clientGlobals.cgFreeParticles .= (p^.cpNext)
              activeParticles <- use $ clientGlobals.cgActiveParticles
              clientGlobals.cgActiveParticles .= Just pRef

              time <- use $ globals.cl.csTime
              r <- Lib.rand
              f <- Lib.randomF
              o1 <- Lib.rand
              o2 <- Lib.rand
              o3 <- Lib.rand
              v1 <- Lib.rand
              v2 <- Lib.rand
              v3 <- Lib.rand

              io $ modifyIORef' pRef (\v -> v { _cpNext = activeParticles
                                              , _cpTime = fromIntegral time
                                              , _cpColor = fromIntegral color + fromIntegral (fromIntegral r `mod` run)
                                              , _cpOrg = org + fmap (fromIntegral . (subtract 16) . (`mod` 32)) (V3 o1 o2 o3)
                                              , _cpVel = fmap (fromIntegral . (subtract 128) . (`mod` 256)) (V3 v1 v2 v3)
                                              , _cpAccel = V3 0 0 (- CLFX.particleGravity)
                                              , _cpAlpha = 1.0
                                              , _cpAlphaVel = (-0.4) / (0.6 + f * 0.2)
                                              })

              addColorExplosionParticles (p^.cpNext) (idx + 1) maxIdx

widowSplash :: V3 Float -> Quake ()
widowSplash _ = do
    io (putStrLn "CLNewFX.widowSplash") >> undefined -- TODO

trackerTrail :: V3 Float -> V3 Float -> Int -> Quake ()
trackerTrail _ _ _ = do
    io (putStrLn "CLNewFX.trackerTrail") >> undefined -- TODO

trackerShell :: V3 Float -> Quake ()
trackerShell _ = do
    io (putStrLn "CLNewFX.trackerShell") >> undefined -- TODO
