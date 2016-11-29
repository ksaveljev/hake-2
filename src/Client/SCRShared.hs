module Client.SCRShared
    ( addDirtyPoint
    , dirtyScreen
    , drawCrosshair
    ) where

import           Control.Lens (use, (^.), (%=), (&), (%~))

import           Client.DirtyT
import           Client.VidDefT
import           QuakeState
import           Types

addDirtyPoint :: Int -> Int -> Quake ()
addDirtyPoint x y =
  scrGlobals.scrDirty %= (\v -> v & x1 %~ (min x)
                                  & x2 %~ (max x)
                                  & y1 %~ (min y)
                                  & y2 %~ (max y))

drawCrosshair :: Quake ()
drawCrosshair = error "SCRShared.drawCrosshair" -- TODO

dirtyScreen :: Quake ()
dirtyScreen =
  do vidDef <- use (globals.gVidDef)
     addDirtyPoint 0 0
     addDirtyPoint ((vidDef^.vdWidth) - 1) ((vidDef^.vdHeight) - 1)
