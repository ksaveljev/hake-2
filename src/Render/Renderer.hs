{-# LANGUAGE TemplateHaskell #-}
module Render.Renderer
  ( module Render.Renderer
  ) where

import Types

import Control.Lens (makeLenses)

makeLenses ''Renderer