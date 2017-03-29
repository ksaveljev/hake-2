{-# LANGUAGE TemplateHaskell #-}
module Render.Renderer ( Renderer(..)
                       , module Client.RefExportT
                       , module Render.Renderer
                       , module Render.RenderAPI
                       ) where

import Control.Lens (makeLenses)

import Types
import Client.RefExportT
import Render.RenderAPI

makeLenses ''Renderer
