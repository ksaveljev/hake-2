module Game.CSurface where

import qualified Data.ByteString as B

data CSurface =
  CSurface { cSurfaceName  :: B.ByteString
           , cSurfaceFlags :: Int
           , cSurfaceValue :: Int
           }
