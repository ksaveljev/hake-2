{-# LANGUAGE TemplateHaskell #-}
module Game.CModelT where

import           Control.Lens (makeLenses)
import           Linear.V3    (V3(..))

import           Types

makeLenses ''CModelT

newCModelT :: CModelT
newCModelT = CModelT
    { _cmMins     = V3 0 0 0
    , _cmMaxs     = V3 0 0 0
    , _cmOrigin   = V3 0 0 0
    , _cmHeadNode = 0
    }
