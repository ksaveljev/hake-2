{-# LANGUAGE TemplateHaskell #-}
module Render.GLPolyT ( GLPolyT(..)
                      , module Render.GLPolyT
                      ) where

import Control.Lens (makeLenses)

import Internal

makeLenses ''GLPolyT

newGLPolyT :: GLPolyT
newGLPolyT = undefined -- TODO
