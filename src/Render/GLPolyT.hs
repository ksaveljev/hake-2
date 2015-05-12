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

maxPolys :: Int
maxPolys = 20000

maxBufferVertices :: Int
maxBufferVertices = 120000

newGLPolyT :: GLPolyT
newGLPolyT = undefined -- TODO
