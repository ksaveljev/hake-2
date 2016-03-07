{-# LANGUAGE TemplateHaskell #-}
module Client.CParticleT
  ( module Client.CParticleT
  ) where

import Types

import Control.Lens (makeLenses)
import Linear (V3(..))

makeLenses ''CParticleT

newCParticleT :: CParticleT
newCParticleT =
  CParticleT { _cpTime     = 0
             , _cpOrg      = V3 0 0 0
             , _cpVel      = V3 0 0 0
             , _cpAccel    = V3 0 0 0
             , _cpColor    = 0
             , _cpAlpha    = 0
             , _cpAlphaVel = 0
             , _cpNext     = Nothing
             }