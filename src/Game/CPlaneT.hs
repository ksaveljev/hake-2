{-# LANGUAGE TemplateHaskell #-}
module Game.CPlaneT where

import Data.Int (Int8)
import Linear.V3 (V3(..))
import Control.Lens (makeLenses)

data CPlaneT =
  CPlaneT { _cpNormal   :: V3 Float
          , _cpDist     :: Float
          , _cpType     :: Int8
          , _cpSignBits :: Int8
          , _cpPad      :: (Int8, Int8)
          }

makeLenses ''CPlaneT

newCPlaneT :: CPlaneT
newCPlaneT =
  CPlaneT { _cpNormal   = V3 0 0 0
          , _cpDist     = 0
          , _cpType     = 0
          , _cpSignBits = 0
          , _cpPad      = (0, 0)
          }
