module Client.Menu where

import qualified Data.ByteString as B

import Quake
import QuakeState
import QCommon.XCommandT

init :: Quake ()

menuAddItem :: MenuFrameworkSReference -> MenuItemReference -> Quake ()

menuCenter :: MenuFrameworkSReference -> Quake ()

menuTallySlots :: MenuFrameworkSReference -> Quake Int

pushMenu :: XCommandT -> KeyFuncT -> Quake ()

menuMainF :: XCommandT

mainDrawF :: XCommandT

mainKeyF :: KeyFuncT

menuGameF :: XCommandT

menuLoadGameF :: XCommandT

menuSaveGameF :: XCommandT

menuJoinServerF :: XCommandT

menuAddressBookF :: XCommandT

menuStartServerF :: XCommandT

menuDMOptionsF :: XCommandT

menuPlayerConfigF :: XCommandT

menuDownloadOptionsF :: XCommandT

menuCreditsF :: XCommandT

menuMultiplayerF :: XCommandT

menuVideoF :: XCommandT

menuOptionsF :: XCommandT

menuKeysF :: XCommandT

menuQuitF :: XCommandT

quitDrawF :: XCommandT

quitKeyF :: KeyFuncT

draw :: Quake ()

keyDown :: Int -> Quake ()

addToServerList :: NetAdrT -> B.ByteString -> Quake ()

drawCursor :: Int -> Int -> Int -> Quake ()

popMenu :: Quake ()

forceMenuOff :: Quake ()

menuAdjustCursor :: MenuFrameworkSReference -> Int -> Quake ()

menuDraw :: MenuFrameworkSReference -> Quake ()

menuSlideItem :: MenuFrameworkSReference -> Int -> Quake ()

menuSelectItem :: MenuFrameworkSReference -> Quake Bool
