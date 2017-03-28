{-# LANGUAGE TemplateHaskell #-}
module Render.RenderAPI ( RenderAPI(..)
                        , module Render.RenderAPI
                        ) where

import Control.Lens (makeLenses)

import Internal

makeLenses ''RenderAPI
