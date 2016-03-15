{-# LANGUAGE TemplateHaskell #-}
module Game.CSurfaceT
  ( module Game.CSurfaceT
  ) where

import           Types

import           Control.Lens (makeLenses)
import qualified Data.ByteString as B

makeLenses ''CSurfaceT

newCSurfaceT :: CSurfaceT
newCSurfaceT =
  CSurfaceT { _csName  = B.empty
            , _csFlags = 0
            , _csValue = 0
            }