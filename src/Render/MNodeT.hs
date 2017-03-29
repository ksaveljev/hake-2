{-# LANGUAGE TemplateHaskell #-}
module Render.MNodeT ( MNodeT(..)
                     , module Render.MNodeT
                     ) where

import Control.Lens (makeLenses)

import Types

makeLenses ''MNodeT
