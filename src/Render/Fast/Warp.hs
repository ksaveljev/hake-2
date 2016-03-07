module Render.Fast.Warp
  ( rSetSky
  ) where

import           Types

import qualified Data.ByteString as B
import           Linear (V3)

rSetSky :: B.ByteString -> Float -> V3 Float -> Quake ()
rSetSky = error "Warp.rSetSky" -- TODO