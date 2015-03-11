{-# LANGUAGE TemplateHaskell #-}
module Game.EntityStateT where

import Control.Lens (makeLenses)

import Internal

makeLenses ''EntityStateT
