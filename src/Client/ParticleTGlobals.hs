{-# LANGUAGE TemplateHaskell #-}
module Client.ParticleTGlobals
  ( module Client.ParticleTGlobals
  ) where

import           Types

import           Control.Lens (makeLenses)
import qualified Data.Vector.Unboxed as UV

makeLenses ''ParticleTGlobals

initialParticleTGlobals :: ParticleTGlobals
initialParticleTGlobals =
  ParticleTGlobals { _pColorTable  = UV.replicate 256 0
                   }
