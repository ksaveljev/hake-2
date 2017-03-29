{-# LANGUAGE TemplateHaskell #-}
module Render.MLeafT ( MLeafT(..)
                     , module Render.MLeafT
                     ) where

import Control.Lens (makeLenses)

import Types

makeLenses ''MLeafT
