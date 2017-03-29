module Client.V where

import Data.IORef (IORef)
import Linear (V3)

import Types
import QuakeState
import QCommon.XCommandT

gunNextF :: XCommandT

gunPrevF :: XCommandT

gunModelF :: XCommandT

viewPosF :: XCommandT

init :: Quake ()

renderView :: Float -> Quake ()

clearScene :: Quake ()

testParticles :: Quake ()

testEntities :: Quake ()

testLights :: Quake ()

addParticle :: V3 Float -> Int -> Float -> Quake ()

addEntity :: IORef EntityT -> Quake ()

addLight :: V3 Float -> Float -> Float -> Float -> Float -> Quake ()

addLightStyle :: Int -> Float -> Float -> Float -> Quake ()
