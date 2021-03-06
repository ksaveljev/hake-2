{-# LANGUAGE TemplateHaskell #-}
module Client.CLightStyleT where

import           Control.Lens        (makeLenses)
import qualified Data.Vector.Unboxed as UV
import           Linear              (V3(..))

import qualified Constants
import           Types

makeLenses ''CLightStyleT

newCLightStyleT :: CLightStyleT
newCLightStyleT = CLightStyleT
    { _clsLength = 0
    , _clsValue  = V3 0 0 0
    , _clsMap    = UV.replicate Constants.maxQPath 0
    }
