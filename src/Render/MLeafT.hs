{-# LANGUAGE TemplateHaskell #-}
module Render.MLeafT ( MLeafT(..)
                     , module Render.MLeafT
                     ) where

import Control.Lens (makeLenses)

import Internal

makeLenses ''MLeafT

newMLeafT :: MLeafT
newMLeafT = undefined -- TODO
