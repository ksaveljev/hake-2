{-# LANGUAGE TemplateHaskell #-}
module Client.VGlobals ( module Client.VGlobals
                       , module Client.DLightT
                       , module Client.EntityT
                       , module Client.LightStyleT
                       ) where

import Control.Lens (makeLenses)
import Data.IORef (newIORef)
import System.IO.Unsafe (unsafePerformIO)
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
           , _vgREntities     = unsafePerformIO $ V.replicateM Constants.maxEntities (newIORef newEntityT)
           , _vgRDLights      = V.replicate Constants.maxDLights newDLightT
           }
