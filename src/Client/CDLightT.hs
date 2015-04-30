{-# LANGUAGE TemplateHaskell #-}
module Client.CDLightT ( CDLightT
                       , module Client.CDLightT
                       ) where

import Control.Lens (makeLenses)
import Linear (V3(..))

import Internal

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
