{-# LANGUAGE TemplateHaskell #-}
module Game.MapSurfaceT where

import Control.Lens (makeLenses)
import qualified Data.ByteString as B

import Game.CSurfaceT

data MapSurfaceT =
  MapSurfaceT { _msCSurface :: CSurfaceT
              , _msRName    :: B.ByteString
              }

makeLenses ''MapSurfaceT
