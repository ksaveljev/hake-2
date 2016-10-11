{-# LANGUAGE TemplateHaskell #-}
module Render.GLPolyT
    ( module Render.GLPolyT
    ) where

import           Control.Lens (makeLenses)

import           Types

makeLenses ''GLPolyT

newGLPolyT :: GLPolyT
newGLPolyT = GLPolyT
    { _glpNext     = Nothing
    , _glpChain    = Nothing
    , _glpNumVerts = 0
    , _glpFlags    = 0
    , _glpPos      = 0
    }