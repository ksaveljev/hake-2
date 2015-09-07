module Client.CLNewFX where

import Linear (V3)

import Quake
import QuakeState

monsterPlasmaShell :: V3 Float -> Quake ()
monsterPlasmaShell _ = do
    io (putStrLn "CLNewFX.monsterPlasmaShell") >> undefined -- TODO

heatBeam :: V3 Float -> V3 Float -> Quake ()
heatBeam _ _ = do
    io (putStrLn "CLNewFX.heatBeam") >> undefined -- TODO

tagTrail :: V3 Float -> V3 Float -> Float -> Quake ()
tagTrail _ _ _ = do
    io (putStrLn "CLNewFX.tagTrail") >> undefined -- TODO

blasterTrail2 :: V3 Float -> V3 Float -> Quake ()
blasterTrail2 _ _ = do
    io (putStrLn "CLNewFX.blasterTrail2") >> undefined -- TODO
