{-# LANGUAGE TemplateHaskell #-}
module Render.MNodeT ( MNodeT(..)
                     , module Render.MNodeT
                     ) where

import Control.Lens (makeLenses)

import Internal

makeLenses ''MNodeT

newMNodeT :: MNodeT
newMNodeT = undefined -- TODO
