module Render.Fast.Light where

import Control.Lens ((^.), preuse, use, (.=), ix)
import Control.Monad (when, liftM)
import Data.Bits (shiftL)
import Data.Maybe (fromJust)
import Linear (V3)
import qualified Data.Vector as V

import Quake
import QuakeState
import CVarVariables
