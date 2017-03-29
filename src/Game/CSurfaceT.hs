{-# LANGUAGE TemplateHaskell #-}
module Game.CSurfaceT where

import           Control.Lens    (makeLenses)
import qualified Data.ByteString as B

import           Types

makeLenses ''CSurfaceT

newCSurfaceT :: CSurfaceT
newCSurfaceT = CSurfaceT
    { _csName  = ""
    , _csFlags = 0
    , _csValue = 0
    }
