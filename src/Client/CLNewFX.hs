module Client.CLNewFX
    ( blasterParticles2
    , blasterTrail2
    , bubbleTrail2
    , colorExplosionParticles
    , colorFlash
    , debugTrail
    , flashlight
    , forceWall
    , particleSmokeEffect
    , particleSteamEffect
    , tagTrail
    , trackerShell
    , trackerTrail
    , widowSplash
    ) where

import           Control.Lens        (use, (^.), (.=), (&), (.~), (+~), (-~))
import           Data.Bits           ((.&.))
import qualified Data.Vector.Unboxed as UV
import           Linear              (V3(..), dot, normalize, norm, _z)

import           Client.CDLightT
import qualified Client.CLFX         as CLFX
import           Client.ClientStateT
import           Client.CParticleT
import           QuakeRef
import           QuakeState
import           Types
import qualified Util.Lib            as Lib
import qualified Util.Math3D         as Math3D


wsColorTable :: UV.Vector Int
wsColorTable = UV.fromList [ 2 * 8, 13 * 8, 21 * 8, 18 * 8 ]

blasterParticles2 :: V3 Float -> V3 Float -> Int -> Quake ()
blasterParticles2 org dir color = do
    freeParticles <- use (clientGlobals.cgFreeParticles)
    addBlasterParticles2 freeParticles 0 (40 :: Int)
  where
    addBlasterParticles2 Nothing _ _ = return ()
    addBlasterParticles2 (Just pRef) idx maxIdx
        | idx >= maxIdx = return ()
        | otherwise = do
            p <- readRef pRef
            activeParticles <- use (clientGlobals.cgActiveParticles)
            clientGlobals.cgFreeParticles .= (p^.cpNext)
            clientGlobals.cgActiveParticles .= Just pRef
            time <- use (globals.gCl.csTime)
            f <- Lib.randomF
            r <- Lib.rand
            d <- fmap (\v -> fromIntegral (v .&. 15)) Lib.rand
            o1 <- Lib.rand
            o2 <- Lib.rand
            o3 <- Lib.rand
            v1 <- Lib.crandom
            v2 <- Lib.crandom
            v3 <- Lib.crandom
            modifyRef pRef (\v -> v & cpNext     .~ activeParticles
                                    & cpTime     .~ fromIntegral time
                                    & cpAccel    .~ V3 0 0 (- CLFX.particleGravity)
                                    & cpColor    .~ fromIntegral color + fromIntegral (r .&. 7)
                                    & cpOrg      .~ org + fmap (fromIntegral . (subtract 4) . (.&. 7)) (V3 o1 o2 o3) + fmap (* d) dir
                                    & cpVel      .~ fmap (* 30) dir + fmap (* 40) (V3 v1 v2 v3)
                                    & cpAlpha    .~ 1.0
                                    & cpAlphaVel .~ (-1.0) / (0.5 + f * 0.3))
            addBlasterParticles2 (p^.cpNext) (idx + 1) maxIdx

blasterTrail2 :: V3 Float -> V3 Float -> Quake ()
blasterTrail2 start end = do
    freeParticles <- use (clientGlobals.cgFreeParticles)
    addBlasterTrail2 freeParticles (fmap (* 5) (normalize (end - start))) start (norm (end - start))
  where
    addBlasterTrail2 Nothing _ _ _ = return ()
    addBlasterTrail2 (Just pRef) vec move len
        | len <= 0 = return ()
        | otherwise = do
            p <- readRef pRef
            activeParticles <- use (clientGlobals.cgActiveParticles)
            clientGlobals.cgFreeParticles .= (p^.cpNext)
            clientGlobals.cgActiveParticles .= Just pRef
            time <- use (globals.gCl.csTime)
            f <- Lib.randomF
            o1 <- Lib.crandom
            o2 <- Lib.crandom
            o3 <- Lib.crandom
            v1 <- Lib.crandom
            v2 <- Lib.crandom
            v3 <- Lib.crandom
            modifyRef pRef (\v -> v & cpNext     .~ activeParticles
                                    & cpTime     .~ fromIntegral time
                                    & cpAccel    .~ V3 0 0 0
                                    & cpAlpha    .~ 1.0
                                    & cpAlphaVel .~ (-1.0) / (0.3 + f * 0.2)
                                    & cpColor    .~ 0xD0
                                    & cpOrg      .~ move + V3 o1 o2 o3
                                    & cpVel      .~ fmap (* 5) (V3 v1 v2 v3))
            addBlasterTrail2 (p^.cpNext) vec (move + vec) (len - 5)

bubbleTrail2 :: V3 Float -> V3 Float -> Float -> Quake ()
bubbleTrail2 start end dist = do
    freeParticles <- use (clientGlobals.cgFreeParticles)
    addBubbleTrail2 freeParticles (fmap (* dist) (normalize (end - start))) start 0 (norm (end - start))
  where
    addBubbleTrail2 Nothing _ _ _ _ = return ()
    addBubbleTrail2 (Just pRef) vec move i len
        | i >= len = return ()
        | otherwise = do
            p <- readRef pRef
            activeParticles <- use (clientGlobals.cgActiveParticles)
            clientGlobals.cgFreeParticles .= (p^.cpNext)
            clientGlobals.cgActiveParticles .= Just pRef
            time <- use (globals.gCl.csTime)
            f <- Lib.randomF
            r <- Lib.rand
            o1 <- Lib.crandom
            o2 <- Lib.crandom
            o3 <- Lib.crandom
            v1 <- Lib.crandom
            v2 <- Lib.crandom
            v3 <- Lib.crandom
            modifyRef pRef (\v -> v & cpNext     .~ activeParticles
                                    & cpTime     .~ fromIntegral time
                                    & cpAccel    .~ V3 0 0 0
                                    & cpAlpha    .~ 1.0
                                    & cpAlphaVel .~ (-1.0) / (1.0 + f * 0.1)
                                    & cpColor    .~ 4 + fromIntegral (r .&. 7)
                                    & cpOrg      .~ ((move + fmap (* 2) (V3 o1 o2 o3)) & _z -~ 4)
                                    & cpVel      .~ ((fmap (* 10) (V3 v1 v2 v3)) & _z +~ 20))
            addBubbleTrail2 (p^.cpNext) vec (move + vec) (i + dist) len

colorExplosionParticles :: V3 Float -> Int -> Int -> Quake ()
colorExplosionParticles org color run = do
    freeParticles <- use (clientGlobals.cgFreeParticles)
    addColorExplosionParticles freeParticles 0 (128 :: Int)
  where
    addColorExplosionParticles Nothing _ _ = return ()
    addColorExplosionParticles (Just pRef) idx maxIdx
        | idx >= maxIdx = return ()
        | otherwise = do
            p <- readRef pRef
            activeParticles <- use (clientGlobals.cgActiveParticles)
            clientGlobals.cgFreeParticles .= (p^.cpNext)
            clientGlobals.cgActiveParticles .= Just pRef
            time <- use (globals.gCl.csTime)
            r <- Lib.rand
            f <- Lib.randomF
            o1 <- Lib.rand
            o2 <- Lib.rand
            o3 <- Lib.rand
            v1 <- Lib.rand
            v2 <- Lib.rand
            v3 <- Lib.rand
            modifyRef pRef (\v -> v & cpNext     .~ activeParticles
                                    & cpTime     .~ fromIntegral time
                                    & cpColor    .~ fromIntegral color + fromIntegral (fromIntegral r `mod` run)
                                    & cpOrg      .~ org + fmap (fromIntegral . (subtract 16) . (`mod` 32)) (V3 o1 o2 o3)
                                    & cpVel      .~ fmap (fromIntegral . (subtract 128) . (`mod` 256)) (V3 v1 v2 v3)
                                    & cpAccel    .~ V3 0 0 (- CLFX.particleGravity)
                                    & cpAlpha    .~ 1.0
                                    & cpAlphaVel .~ (-0.4) / (0.6 + f * 0.2))
            addColorExplosionParticles (p^.cpNext) (idx + 1) maxIdx

colorFlash :: V3 Float -> Int -> Int -> Float -> Float -> Float -> Quake ()
colorFlash pos ent intensity r g b = do
    -- TODO: if ((Globals.vidref_val == Defines.VIDREF_SOFT)
    time <- use (globals.gCl.csTime)
    dlRef <- CLFX.allocDLight ent
    modifyRef dlRef (\v -> v & cdlOrigin   .~ pos
                             & cdlRadius   .~ fromIntegral intensity'
                             & cdlMinLight .~ 250
                             & cdlDie      .~ fromIntegral time + 100
                             & cdlColor    .~ V3 r' g' b')
  where
    (r', g', b', intensity') = if r < 0 || g < 0 || b < 0
                                   then (-r, -g, -b, -intensity)
                                   else (r, g, b, intensity)

debugTrail :: V3 Float -> V3 Float -> Quake ()
debugTrail = error "CLNewFX.debugTrail" -- TODO

flashlight :: Int -> V3 Float -> Quake ()
flashlight ent pos = do
    dlRef <- CLFX.allocDLight ent
    time <- use (globals.gCl.csTime)
    modifyRef dlRef (\v -> v & cdlOrigin   .~ pos
                             & cdlRadius   .~ 400
                             & cdlMinLight .~ 250
                             & cdlDie      .~ fromIntegral time + 100
                             & cdlColor    .~ V3 1 1 1)

forceWall :: V3 Float -> V3 Float -> Int -> Quake ()
forceWall start end color = do
    freeParticles <- use (clientGlobals.cgFreeParticles)
    addForceWall freeParticles (fmap (* 4) (normalize (end - start))) start (norm (end - start))
  where
    addForceWall Nothing _ _ _ = return ()
    addForceWall (Just pRef) vec move len
        | len <= 0 = return ()
        | otherwise = do
            f <- Lib.randomF
            doAddForceWall pRef vec move len f
    doAddForceWall pRef vec move len f
        | f > 0.3 = do
            p <- readRef pRef
            activeParticles <- use (clientGlobals.cgActiveParticles)
            clientGlobals.cgFreeParticles .= (p^.cpNext)
            clientGlobals.cgActiveParticles .= Just pRef
            time <- use (globals.gCl.csTime)
            f' <- Lib.randomF
            o1 <- Lib.crandom
            o2 <- Lib.crandom
            o3 <- Lib.crandom
            v2 <- Lib.crandom
            modifyRef pRef (\v -> v & cpNext     .~ activeParticles
                                    & cpTime     .~ fromIntegral time
                                    & cpAccel    .~ V3 0 0 0
                                    & cpAlpha    .~ 1.0
                                    & cpAlphaVel .~ (-1.0) / (3.0 + f' * 0.5)
                                    & cpColor    .~ fromIntegral color
                                    & cpOrg      .~ move + fmap (* 3) (V3 o1 o2 o3)
                                    & cpVel      .~ V3 0 0 ((-40) - v2 * 10))
            addForceWall (p^.cpNext) vec (move + vec) (len - 4)
        | otherwise =
            addForceWall (Just pRef) vec (move + vec) (len - 4)

particleSmokeEffect :: V3 Float -> V3 Float -> Int -> Int -> Int -> Quake ()
particleSmokeEffect org dir color count magnitude = do
    let (r, u) = Math3D.makeNormalVectors dir
    freeParticles <- use (clientGlobals.cgFreeParticles)
    addParticleSmokeEffect freeParticles r u 0
  where
    magnitude' = fromIntegral magnitude
    addParticleSmokeEffect Nothing _ _ _ = return ()
    addParticleSmokeEffect (Just pRef) r u idx
        | idx >= count = return ()
        | otherwise = do
            p <- readRef pRef
            activeParticles <- use (clientGlobals.cgActiveParticles)
            clientGlobals.cgFreeParticles .= (p^.cpNext)
            clientGlobals.cgActiveParticles .= Just pRef
            time <- use (globals.gCl.csTime)
            r' <- Lib.rand
            o1 <- Lib.crandom
            o2 <- Lib.crandom
            o3 <- Lib.crandom
            f <- Lib.randomF
            d <- fmap (\v -> v * magnitude' / 3) Lib.crandom
            d' <- fmap (\v -> v * magnitude' / 3) Lib.crandom
            modifyRef pRef (\v -> v & cpNext     .~ activeParticles
                                    & cpTime     .~ fromIntegral time
                                    & cpColor    .~ fromIntegral color + fromIntegral (r' .&. 7)
                                    & cpOrg      .~ org + fmap (* (magnitude' * 0.1)) (V3 o1 o2 o3)
                                    & cpVel      .~ fmap (* magnitude') dir + fmap (* d) r + fmap (* d') u
                                    & cpAccel    .~ V3 0 0 0
                                    & cpAlpha    .~ 1.0
                                    & cpAlphaVel .~ (-1.0) / (0.5 + f * 0.3))
            addParticleSmokeEffect (p^.cpNext) r u (idx + 1)

particleSteamEffect :: V3 Float -> V3 Float -> Int -> Int -> Int -> Quake ()
particleSteamEffect org dir color count magnitude = do
    let (r, u) = Math3D.makeNormalVectors dir
    freeParticles <- use (clientGlobals.cgFreeParticles)
    addParticleSteamEffect freeParticles r u 0
  where
    magnitude' = fromIntegral magnitude
    addParticleSteamEffect Nothing _ _ _ = return ()
    addParticleSteamEffect (Just pRef) r u idx
        | idx >= count = return ()
        | otherwise = do
            p <- readRef pRef
            activeParticles <- use (clientGlobals.cgActiveParticles)
            clientGlobals.cgFreeParticles .= (p^.cpNext)
            clientGlobals.cgActiveParticles .= Just pRef
            time <- use (globals.gCl.csTime)
            r' <- Lib.rand
            f <- Lib.randomF
            o1 <- Lib.crandom
            o2 <- Lib.crandom
            o3 <- Lib.crandom
            d <- fmap (\v -> v * magnitude' / 3) Lib.crandom
            d' <- fmap (\v -> v * magnitude' / 3) Lib.crandom
            modifyRef pRef (\v -> v & cpNext     .~ activeParticles
                                    & cpTime     .~ fromIntegral time
                                    & cpColor    .~ fromIntegral color + fromIntegral (r' .&. 7)
                                    & cpOrg      .~ org + fmap (* (magnitude' * 0.1)) (V3 o1 o2 o3)
                                    & cpVel      .~ fmap (* magnitude') dir + fmap (* d) r + fmap (* d') u
                                    & cpAccel    .~ V3 0 0 (- CLFX.particleGravity / 2)
                                    & cpAlpha    .~ 1.0
                                    & cpAlphaVel .~ (-1.0) / (0.5 + f * 0.3))
            addParticleSteamEffect (p^.cpNext) r u (idx + 1)

tagTrail :: V3 Float -> V3 Float -> Float -> Quake ()
tagTrail start end color = do
    freeParticles <- use (clientGlobals.cgFreeParticles)
    addTagTrail freeParticles (fmap (* 5) (normalize (end - start))) start (norm (end - start))
  where
    addTagTrail Nothing _ _ _ = return ()
    addTagTrail (Just pRef) vec move len
        | len < 0 = return ()
        | otherwise = do
            p <- readRef pRef
            activeParticles <- use (clientGlobals.cgActiveParticles)
            clientGlobals.cgFreeParticles .= (p^.cpNext)
            clientGlobals.cgActiveParticles .= Just pRef
            time <- use (globals.gCl.csTime)
            f <- Lib.randomF
            o1 <- Lib.crandom
            o2 <- Lib.crandom
            o3 <- Lib.crandom
            v1 <- Lib.crandom
            v2 <- Lib.crandom
            v3 <- Lib.crandom
            modifyRef pRef (\v -> v & cpNext     .~ activeParticles
                                    & cpAccel    .~ V3 0 0 0
                                    & cpTime     .~ fromIntegral time
                                    & cpAlpha    .~ 1.0
                                    & cpAlphaVel .~ (-1.0) / (0.8 + f * 0.2)
                                    & cpColor    .~ color
                                    & cpOrg      .~ move + fmap (* 16) (V3 o1 o2 o3)
                                    & cpVel      .~ fmap (* 5) (V3 v1 v2 v3))
            addTagTrail (p^.cpNext) vec (move + vec) (len - 5)

trackerShell :: V3 Float -> Quake ()
trackerShell origin = do
    freeParticles <- use (clientGlobals.cgFreeParticles)
    addTrackerShell freeParticles 0 (300 :: Int)
  where
    addTrackerShell Nothing _ _ = return ()
    addTrackerShell (Just pRef) idx maxIdx
        | idx >= maxIdx = return ()
        | otherwise = do
            p <- readRef pRef
            activeParticles <- use (clientGlobals.cgActiveParticles)
            clientGlobals.cgFreeParticles .= (p^.cpNext)
            clientGlobals.cgActiveParticles .= Just pRef
            time <- use (globals.gCl.csTime)
            d1 <- Lib.crandom
            d2 <- Lib.crandom
            d3 <- Lib.crandom
            modifyRef pRef (\v -> v & cpNext     .~ activeParticles
                                    & cpTime     .~ fromIntegral time
                                    & cpAccel    .~ V3 0 0 0
                                    & cpAlpha    .~ 1.0
                                    & cpAlphaVel .~ - CLFX.instantParticle
                                    & cpColor    .~ 0
                                    & cpOrg      .~ origin + fmap (* 40) (normalize (V3 d1 d2 d3)))
            addTrackerShell (p^.cpNext) (idx + 1) maxIdx

trackerTrail :: V3 Float -> V3 Float -> Int -> Quake ()
trackerTrail start end particleColor = do
    let angleDir = Math3D.vectorAngles (normalize (end - start))
        (forward, _, up) = Math3D.angleVectors angleDir True True True
    freeParticles <- use (clientGlobals.cgFreeParticles)
    addTrackerTrail freeParticles (fmap (* 3) (normalize (end - start))) start forward up (norm (end - start))
  where
    addTrackerTrail Nothing _ _ _ _ _ = return ()
    addTrackerTrail (Just pRef) vec move forward up len
        | len <= 0 = return ()
        | otherwise = do
            p <- readRef pRef
            activeParticles <- use (clientGlobals.cgActiveParticles)
            clientGlobals.cgFreeParticles .= (p^.cpNext)
            clientGlobals.cgActiveParticles .= Just pRef
            time <- use (globals.gCl.csTime)
            modifyRef pRef (\v -> v & cpNext     .~ activeParticles
                                    & cpAccel    .~ V3 0 0 0
                                    & cpTime     .~ fromIntegral time
                                    & cpAlpha    .~ 1.0
                                    & cpAlphaVel .~ -2.0
                                    & cpColor    .~ fromIntegral particleColor
                                    & cpOrg      .~ move + fmap (* (8 * cos (move `dot` forward))) up
                                    & cpVel      .~ V3 0 0 5)
            addTrackerTrail (p^.cpNext) vec (move + vec) forward up (len - 3)

widowSplash :: V3 Float -> Quake ()
widowSplash org = do
    freeParticles <- use (clientGlobals.cgFreeParticles)
    addWidowSplash freeParticles 0 (256 :: Int)
  where
    addWidowSplash Nothing _ _ = return ()
    addWidowSplash (Just pRef) idx maxIdx
        | idx >= maxIdx = return ()
        | otherwise = do
            p <- readRef pRef
            activeParticles <- use (clientGlobals.cgActiveParticles)
            clientGlobals.cgFreeParticles .= (p^.cpNext)
            clientGlobals.cgActiveParticles .= Just pRef
            v3o <- use (globals.gVec3Origin)
            time <- use (globals.gCl.csTime)
            r <- Lib.rand
            d1 <- Lib.crandom
            d2 <- Lib.crandom
            d3 <- Lib.crandom
            f <- Lib.randomF
            let dir = normalize (V3 d1 d2 d3)
            modifyRef pRef (\v -> v & cpNext     .~ activeParticles
                                    & cpTime     .~ fromIntegral time
                                    & cpColor    .~ fromIntegral (wsColorTable UV.! fromIntegral (r .&. 3))
                                    & cpOrg      .~ org + fmap (* 45) dir
                                    & cpVel      .~ v3o + fmap (* 40) dir
                                    & cpAccel    .~ V3 0 0 (p^.cpAccel._z)
                                    & cpAlpha    .~ 1.0
                                    & cpAlphaVel .~ (-0.8) / (0.5 + f * 0.3))
            addWidowSplash (p^.cpNext) (idx + 1) maxIdx
