{-# LANGUAGE TemplateHaskell #-}
module Client.CParticleT ( CParticleT(..)
                         , module Client.CParticleT
                         ) where

import Linear.V3 (V3(..))
import Control.Lens (makeLenses)

import Types

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
