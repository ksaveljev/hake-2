{-# LANGUAGE TemplateHaskell #-}
module Client.ParticleT where

import Data.Int (Int8)
import Control.Lens (makeLenses)
import qualified Data.Vector.Unboxed as UV

data ParticleT =
  ParticleT { _pColors :: UV.Vector Int8
            , _pVertexArray :: UV.Vector Float
            , _pColorTable :: UV.Vector Int
            , _pColorArray :: UV.Vector Int
            }

makeLenses ''ParticleT
