{-# LANGUAGE TemplateHaskell #-}
module Game.PMoveT ( PMoveT(..)
                   , module Game.PMoveT
                   ) where
  
import Linear.V3 (V3)
import Control.Lens (makeLenses)
import qualified Data.Vector.Unboxed as UV

import Internal

{-
TODO:
PMF_DUCKED = 1
PMF_JUMP_HELD = 2
PMF_ON_GROUND = 4
PMF_TIME_WATERJUMP = 8
PMF_TIME_LAND = 16
PMF_TIME_TELEPORT = 32
PMF_NO_PREDICTION = 64
-}

makeLenses ''PMoveT
