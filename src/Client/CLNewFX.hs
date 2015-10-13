module Client.CLNewFX where

import Control.Lens (use, (^.), (.=))
import Data.IORef (IORef, readIORef, modifyIORef')
import Linear (V3(..), normalize)

import Quake
import QuakeState
import qualified Client.CLFX as CLFX
import qualified Util.Lib as Lib

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
heatBeam _ _ = do
    io (putStrLn "CLNewFX.heatBeam") >> undefined -- TODO

tagTrail :: V3 Float -> V3 Float -> Float -> Quake ()
tagTrail _ _ _ = do
    io (putStrLn "CLNewFX.tagTrail") >> undefined -- TODO

blasterTrail2 :: V3 Float -> V3 Float -> Quake ()
blasterTrail2 _ _ = do
    io (putStrLn "CLNewFX.blasterTrail2") >> undefined -- TODO

blasterParticles2 :: V3 Float -> V3 Float -> Int -> Quake ()
blasterParticles2 _ _ _ = do
    io (putStrLn "CLNewFX.blasterParticles2") >> undefined -- TODO

debugTrail :: V3 Float -> V3 Float -> Quake ()
debugTrail _ _ = do
    io (putStrLn "CLNewFX.debugTrail") >> undefined -- TODO

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
