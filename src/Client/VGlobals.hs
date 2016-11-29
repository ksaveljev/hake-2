{-# LANGUAGE TemplateHaskell #-}
module Client.VGlobals
  ( module Client.VGlobals
  ) where

import           Client.EntityT (newEntityT)
import           Client.LightStyleT (newLightStyleT)
import           Client.DLightT (newDLightT)
import qualified Constants
import           Types

import           Control.Lens (makeLenses)
import qualified Data.Vector as V

makeLenses ''VGlobals

initialVGlobals :: VGlobals
initialVGlobals =
  VGlobals { _vgNumDLights   = 0
           , _vgNumEntities  = 0
           , _vgNumParticles = 0
           , _vgLightStyles  = V.generate Constants.maxLightStyles Ref
           , _vgDLights      = V.generate Constants.maxDLights Ref
           , _vgEntities     = V.generate Constants.maxEntities Ref
           }