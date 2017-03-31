module Server.SV where

import Linear (V3)

import Types
import QuakeState

physicsPusher :: Ref EdictT -> Quake ()

physicsNone :: Ref EdictT -> Quake ()

physicsNoClip :: Ref EdictT -> Quake ()

physicsStep :: Ref EdictT -> Quake ()

physicsToss :: Ref EdictT -> Quake ()

push :: Ref EdictT -> V3 Float -> V3 Float -> Quake Bool

moveStep :: Ref EdictT -> V3 Float -> Bool -> Quake Bool

stepDirection :: Ref EdictT -> Float -> Float -> Quake Bool

closeEnough :: Ref EdictT -> Ref EdictT -> Float -> Quake Bool

newChaseDir :: Ref EdictT -> Maybe (Ref EdictT) -> Float -> Quake ()
