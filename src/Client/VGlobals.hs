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

import Types
import Client.DLightT
import Client.EntityT
import Client.LightStyleT
import qualified Constants

makeLenses ''VGlobals

initialVGlobals :: VGlobals
initialVGlobals =
  VGlobals { _vgNumDLights   = 0
           , _vgNumEntities  = 0
           , _vgNumParticles = 0
           , _vgLightStyles  = V.replicate Constants.maxLightStyles newLightStyleT
           , _vgEntities     = unsafePerformIO $ V.replicateM Constants.maxEntities (newIORef newEntityT)
           , _vgDLights      = V.replicate Constants.maxDLights newDLightT
           }
