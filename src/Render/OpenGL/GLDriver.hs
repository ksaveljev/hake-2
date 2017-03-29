{-# LANGUAGE TemplateHaskell #-}
module Render.OpenGL.GLDriver ( GLDriver(..)
                              , module Render.OpenGL.GLDriver
                              ) where

import Control.Lens (makeLenses)

import Types

makeLenses ''GLDriver
