{-# LANGUAGE TemplateHaskell #-}
module Client.VGlobals where

import Control.Lens (makeLenses)
import qualified Data.Vector as V

import Internal
import Client.DLightT
import Client.EntityT
import Client.LightStyleT
import qualified Constants

makeLenses ''VGlobals

initialVGlobals :: VGlobals
initialVGlobals =
  VGlobals { _vgRNumDLights   = 0
           , _vgRNumEntities  = 0
           , _vgRNumParticles = 0
           , _vgRLightStyles  = V.replicate Constants.maxLightStyles newLightStyleT
           , _vgREntities     = V.replicate Constants.maxEntities newEntityT
           , _vgRDLights      = V.replicate Constants.maxDLights newDLightT
           }
