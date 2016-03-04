{-# LANGUAGE TemplateHaskell #-}
module Render.RenderAPI
  ( module Render.RenderAPI
  ) where

import Types

import Control.Lens (makeLenses)

makeLenses ''RenderAPI