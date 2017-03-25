{-# LANGUAGE TemplateHaskell #-}
module Game.CPlaneT
    ( module Game.CPlaneT
    ) where

import           Control.Lens (makeLenses)
import           Linear (V3(..))

import           Types

makeLenses ''CPlaneT

newCPlaneT :: CPlaneT
newCPlaneT = CPlaneT
    { _cpNormal   = V3 0 0 0
    , _cpDist     = 0
    , _cpType     = 0
    , _cpSignBits = 0
    , _cpPad      = (0, 0)
    }