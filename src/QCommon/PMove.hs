module QCommon.PMove where

import Linear.V3 (V3(..))
import qualified Data.Vector.Unboxed as UV

-- try all single bits first
jitterBits :: UV.Vector Int
jitterBits = UV.fromList [ 0, 4, 1, 2, 3, 5, 6, 7 ]

offset :: V3 Int
offset = V3 0 (-1) 1
