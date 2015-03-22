{-# LANGUAGE TemplateHaskell #-}
module Render.MVertexT where

import Linear.V3 (V3)
import Control.Lens (makeLenses)

data MVertexT =
  MVertexT { _mvPosition :: V3 Float
           }

makeLenses ''MVertexT

newMVertexT :: MVertexT
newMVertexT = undefined -- TODO
