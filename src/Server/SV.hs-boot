module Server.SV where

import Linear (V3)

import Quake
import QuakeState

physicsPusher :: EdictReference -> Quake ()

physicsNone :: EdictReference -> Quake ()

physicsNoClip :: EdictReference -> Quake ()

physicsStep :: EdictReference -> Quake ()

physicsToss :: EdictReference -> Quake ()

push :: EdictReference -> V3 Float -> V3 Float -> Quake Bool
