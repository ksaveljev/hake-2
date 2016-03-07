{-# LANGUAGE TemplateHaskell #-}
module Client.CDLightT
  ( module Client.CDLightT
  ) where

import Types

import Control.Lens (makeLenses)
import Linear (V3(..))

makeLenses ''CDLightT

newCDLightT :: CDLightT
newCDLightT =
  CDLightT { _cdlKey      = 0
           , _cdlColor    = V3 0 0 0
           , _cdlOrigin   = V3 0 0 0
           , _cdlRadius   = 0
           , _cdlDie      = 0
           , _cdlMinLight = 0
           }