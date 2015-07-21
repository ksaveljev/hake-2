{-# LANGUAGE TemplateHaskell #-}
module Render.GLPolyT ( GLPolyT(..)
                      , module Render.GLPolyT
                      ) where

import Control.Lens (makeLenses)

import Internal

makeLenses ''GLPolyT

maxVertices :: Int
maxVertices = 64

stride :: Int
stride = 7

byteStride :: Int
byteStride = 7 * 4 -- 4 = SIZEOF_FLOAT

maxPolys :: Int
maxPolys = 20000

maxBufferVertices :: Int
maxBufferVertices = 120000

newGLPolyT :: GLPolyT
newGLPolyT =
  GLPolyT { _glpNext           = Nothing
          , _glpChain          = Nothing
          , _glpNumVerts       = 0
          , _glpFlags          = 0
          , _glpPos            = 0
          }
