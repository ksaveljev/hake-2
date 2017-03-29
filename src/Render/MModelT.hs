{-# LANGUAGE TemplateHaskell #-}
module Render.MModelT where

import Linear.V3 (V3)
import Control.Lens (makeLenses)

import Types

makeLenses ''MModelT
