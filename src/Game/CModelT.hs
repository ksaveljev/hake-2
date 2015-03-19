{-# LANGUAGE TemplateHaskell #-}
module Game.CModelT where

import Linear.V3 (V3(..))
import Control.Lens (makeLenses)

data CModelT =
  CModelT { _cmMins     :: V3 Float
          , _cmMaxs     :: V3 Float
          , _cmOrigin   :: V3 Float
          , _cmHeadNode :: Int
          }

makeLenses ''CModelT

newCModelT :: CModelT
newCModelT =
  CModelT { _cmMins     = V3 0 0 0
          , _cmMaxs     = V3 0 0 0
          , _cmOrigin   = V3 0 0 0
          , _cmHeadNode = 0
          }
