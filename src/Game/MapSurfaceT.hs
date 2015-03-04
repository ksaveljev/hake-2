module Game.MapSurfaceT where

import qualified Data.ByteString as B

import Game.CSurfaceT

data MapSurfaceT =
  MapSurfaceT { msCSurface :: CSurfaceT
              , msRName    :: B.ByteString
              }
