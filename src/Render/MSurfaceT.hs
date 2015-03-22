{-# LANGUAGE TemplateHaskell #-}
module Render.MSurfaceT ( MSurfaceT(..)
                        , module Render.MSurfaceT
                        ) where

import Control.Lens (makeLenses)

import Internal

makeLenses ''MSurfaceT

newMSurfaceT :: MSurfaceT
newMSurfaceT = undefined -- TODO
