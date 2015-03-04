module Game.MapSurface where

import qualified Data.ByteString as B

import Game.CSurface

data MapSurface =
  MapSurface { mapSurfaceCSurface :: CSurface
             , mapSurfaceRName    :: B.ByteString
             }
