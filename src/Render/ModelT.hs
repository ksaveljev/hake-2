{-# LANGUAGE TemplateHaskell #-}
module Render.ModelT ( ModelT(..)
                     , module Render.ModelT
                     ) where

import Control.Lens (makeLenses)

import Internal

makeLenses ''ModelT

newModelT :: ModelT
newModelT = undefined -- TODO
