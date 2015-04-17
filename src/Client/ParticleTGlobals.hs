{-# LANGUAGE TemplateHaskell #-}
module Client.ParticleTGlobals ( module Client.ParticleTGlobals
                               ) where

import Control.Lens (makeLenses)
import qualified Data.Vector.Unboxed as UV

import Internal

makeLenses ''ParticleTGlobals

initialParticleTGlobals :: ParticleTGlobals
initialParticleTGlobals =
  ParticleTGlobals { _pColorTable = UV.replicate 256 0
                   }
