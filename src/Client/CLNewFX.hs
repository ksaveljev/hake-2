module Client.CLNewFX where

import Control.Lens (use, (^.), (.=))
import Control.Monad (unless)
import Data.Bits ((.&.))
import Data.IORef (IORef, readIORef, modifyIORef')
import Linear (V3(..), normalize, norm)

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
flashlight _ _ = do
    io (putStrLn "CLNewFX.flashlight") >> undefined -- TODO

forceWall :: V3 Float -> V3 Float -> Int -> Quake ()
forceWall _ _ _ = do
    io (putStrLn "CLNewFX.forceWall") >> undefined -- TODO

particleSteamEffect :: V3 Float -> V3 Float -> Int -> Int -> Int -> Quake ()
particleSteamEffect _ _ _ _ _ = do
    io (putStrLn "CLNewFX.particleSteamEffect") >> undefined -- TODO

bubbleTrail2 :: V3 Float -> V3 Float -> Int -> Quake ()
bubbleTrail2 _ _ _ = do
    io (putStrLn "CLNewFX.bubbleTrail2") >> undefined -- TODO

particleSmokeEffect :: V3 Float -> V3 Float -> Int -> Int -> Int -> Quake ()
particleSmokeEffect _ _ _ _ _ = do
    io (putStrLn "CLNewFX.particleSmokeEffect") >> undefined -- TODO

colorFlash :: V3 Float -> Int -> Int -> Float -> Float -> Float -> Quake ()
colorFlash _ _ _ _ _ _ = do
    io (putStrLn "CLNewFX.colorFlash") >> undefined -- TODO

colorExplosionParticles :: V3 Float -> Int -> Int -> Quake ()
colorExplosionParticles _ _ _ = do
    io (putStrLn "CLNewFX.colorExplosionParticles") >> undefined -- TODO

widowSplash :: V3 Float -> Quake ()
widowSplash _ = do
    io (putStrLn "CLNewFX.widowSplash") >> undefined -- TODO

trackerTrail :: V3 Float -> V3 Float -> Int -> Quake ()
trackerTrail _ _ _ = do
    io (putStrLn "CLNewFX.trackerTrail") >> undefined -- TODO

trackerShell :: V3 Float -> Quake ()
trackerShell _ = do
    io (putStrLn "CLNewFX.trackerShell") >> undefined -- TODO
