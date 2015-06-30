module Client.SCR where

import qualified Data.ByteString as B

import Quake
import QCommon.XCommandT

init :: Quake ()

beginLoadingPlaque :: Quake ()

endLoadingPlaque :: Quake ()

updateScreenF :: XCommandT

updateScreen :: Quake ()

updateScreen2 :: Quake ()

timeRefreshF :: XCommandT

loadingF :: XCommandT

sizeUpF :: XCommandT

sizeDownF :: XCommandT

skyF :: XCommandT

runCinematic :: Quake ()

finishCinematic :: Quake ()

runConsole :: Quake ()

calcVrect :: Quake ()

tileClear :: Quake ()

drawStats :: Quake ()

drawLayout :: Quake ()

drawNet :: Quake ()

checkDrawCenterString :: Quake ()

drawFPS :: Quake ()

drawPause :: Quake ()

drawConsole :: Quake ()

drawCinematic :: Quake Bool

drawLoading :: Quake ()

dirtyScreen :: Quake ()

addDirtyPoint :: Int -> Int -> Quake ()

drawCrosshair :: Quake ()

executeLayoutString :: B.ByteString -> Quake ()

drawField :: Int -> Int -> Int -> Int -> Int -> Quake ()

drawHUDString :: B.ByteString -> Int -> Int -> Int -> Int -> Quake ()

drawCenterString :: Quake ()

playCinematic :: B.ByteString -> Quake ()
