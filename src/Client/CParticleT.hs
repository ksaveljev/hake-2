{-# LANGUAGE TemplateHaskell #-}
module Client.CParticleT where

import Linear.V3 (V3)
import Control.Lens (makeLenses)

data CParticleT =
  CParticleT { _cpTime     :: Float
             , _cpOrg      :: V3 Float
             , _cpVel      :: V3 Float
             , _cpAccel    :: V3 Float
             , _cpColor    :: Float
             , _cpAlpha    :: Float
             , _cpAlphaVel :: Float
             }

makeLenses ''CParticleT
