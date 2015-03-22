{-# LANGUAGE TemplateHaskell #-}
module Render.MTexInfoT ( MTexInfoT(..)
                        , module Render.MTexInfoT
                        ) where

import Control.Lens (makeLenses)

import Internal

makeLenses ''MTexInfoT

newMTexInfoT :: MTexInfoT
newMTexInfoT = undefined -- TODO
