{-# LANGUAGE TemplateHaskell #-}
module Client.ParticleTGlobals ( module Client.ParticleTGlobals
                               ) where

import Control.Lens (makeLenses)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Vector.Storable.Mutable as MSV
import qualified Data.Vector.Unboxed as UV

import Types
import qualified Constants

makeLenses ''ParticleTGlobals

initialParticleTGlobals :: ParticleTGlobals
initialParticleTGlobals =
  ParticleTGlobals { _pColorTable  = UV.replicate 256 0
                   , _pVertexArray = unsafePerformIO $ MSV.new (Constants.maxParticles * 3)
                   , _pColorArray  = unsafePerformIO $ MSV.new Constants.maxParticles
                   }
