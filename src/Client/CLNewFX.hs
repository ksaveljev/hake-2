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

import           Linear (V3)

import           Types

blasterParticles2 :: V3 Float -> V3 Float -> Int -> Quake ()
blasterParticles2 = error "CLNewFX.blasterParticles2" -- TODO

blasterTrail2 :: V3 Float -> V3 Float -> Quake ()
blasterTrail2 = error "CLNewFX.blasterTrail2" -- TODO

bubbleTrail2 :: V3 Float -> V3 Float -> Float -> Quake ()
bubbleTrail2 = error "CLNewFX.bubbleTrail2" -- TODO

colorExplosionParticles :: V3 Float -> Int -> Int -> Quake ()
colorExplosionParticles = error "CLNewFX.colorExplosionParticles" -- TODO

colorFlash :: V3 Float -> Int -> Int -> Float -> Float -> Float -> Quake ()
colorFlash = error "CLNewFX.colorFlash" -- TODO

debugTrail :: V3 Float -> V3 Float -> Quake ()
debugTrail = error "CLNewFX.debugTrail" -- TODO

flashlight :: Int -> V3 Float -> Quake ()
flashlight = error "CLNewFX.flashlight" -- TODO

forceWall :: V3 Float -> V3 Float -> Int -> Quake ()
forceWall = error "CLNewFX.forceWall" -- TODO

particleSmokeEffect :: V3 Float -> V3 Float -> Int -> Int -> Int -> Quake ()
particleSmokeEffect = error "CLNewFX.particleSmokeEffect" -- TODO

particleSteamEffect :: V3 Float -> V3 Float -> Int -> Int -> Int -> Quake ()
particleSteamEffect = error "CLNewFX.particleSteamEffect" -- TODO

tagTrail :: V3 Float -> V3 Float -> Float -> Quake ()
tagTrail = error "CLNewFX.tagTrail" -- TODO

trackerShell :: V3 Float -> Quake ()
trackerShell = error "CLNewFX.trackerShell" -- TODO

trackerTrail :: V3 Float -> V3 Float -> Int -> Quake ()
trackerTrail = error "CLNewFX.trackerTrail" -- TODO

widowSplash :: V3 Float -> Quake ()
widowSplash = error "CLNewFX.widowSplash" -- TODO
