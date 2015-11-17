module Client.Menu where

import qualified Data.ByteString as B

import Quake
import QuakeState
import QCommon.XCommandT

init :: Quake ()

addItem :: MenuFrameworkSReference -> MenuItemReference -> Quake ()

center :: MenuFrameworkSReference -> Quake ()

tallySlots :: MenuFrameworkSReference -> Quake Int

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

menuKeyDown :: Int -> Quake ()

addToServerList :: NetAdrT -> B.ByteString -> Quake ()

drawCursor :: Int -> Int -> Int -> Quake ()

popMenu :: Quake ()

forceMenuOff :: Quake ()
