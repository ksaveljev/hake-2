{-# LANGUAGE TemplateHaskell #-}
module Game.CSurfaceT
  ( module Game.CSurfaceT
  ) where

import Types

import Control.Lens (makeLenses)

makeLenses ''CSurfaceT

newCSurfaceT :: CSurfaceT
newCSurfaceT =
  CSurfaceT { _csName  = ""
            , _csFlags = 0
            , _csValue = 0
            }