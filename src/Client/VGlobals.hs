{-# LANGUAGE TemplateHaskell #-}
module Client.VGlobals
    ( module Client.VGlobals
    ) where

import           Control.Lens       (makeLenses)
import           Data.IORef         (newIORef)
import qualified Data.Vector        as V
import           System.IO.Unsafe   (unsafePerformIO)

import           Client.EntityT     (newEntityT)
import           Client.LightStyleT (newLightStyleT)
import           Client.DLightT     (newDLightT)
import qualified Constants
import           Types

makeLenses ''VGlobals

initialVGlobals :: VGlobals
initialVGlobals = VGlobals
    { _vgNumDLights   = 0
    , _vgNumEntities  = 0
    , _vgNumParticles = 0
    , _vgLightStyles  = V.replicate Constants.maxLightStyles newLightStyleT
    , _vgDLights      = V.replicate Constants.maxDLights newDLightT
    , _vgEntities     = unsafePerformIO (V.replicateM Constants.maxEntities (newIORef newEntityT))
    }