module Client.V where

import Linear (V3)

import Quake
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

addEntity :: EntityT -> Quake ()

addLight :: V3 Float -> Float -> Float -> Float -> Float -> Quake ()
