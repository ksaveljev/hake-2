{-# LANGUAGE TemplateHaskell #-}
module Render.ImageT ( ImageT(..)
                     , module Render.ImageT
                     ) where

import Control.Lens (makeLenses)

import Internal

makeLenses ''ImageT

newImageT :: ImageT
newImageT = undefined -- TODO
