module Game.CPlane where

import Data.Int (Int8)
import Linear.V3 (V3)

data CPlane =
  CPlane { cPlaneNormal   :: V3 Float
         , cPlaneDist     :: Float
         , cPlaneType     :: Int8
         , cPlaneSignBits :: Int8
         , cPlanePad      :: (Int8, Int8)
         }
