module Client.V where

import Quake
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
