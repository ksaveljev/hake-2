{-# LANGUAGE TemplateHaskell #-}
module Render.Renderer
    ( module Render.Renderer
    ) where

import           Control.Lens (makeLenses)

import           Types

makeLenses ''Renderer