module Game.CModel where

import Linear.V3 (V3)

data CModel = CModel { cModelMins     :: V3 Float
                     , cModelMaxs     :: V3 Float
                     , cModelOrigin   :: V3 Float
                     , cModelHeadNode :: Int
                     }
