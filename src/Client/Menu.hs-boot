module Client.Menu where

import qualified Data.ByteString as B

import Types
import QuakeState
import QCommon.XCommandT

init :: Quake ()

menuAddItem :: Ref MenuFrameworkS -> MenuItemRef -> Quake ()

menuCenter :: Ref MenuFrameworkS -> Quake ()

menuTallySlots :: Ref MenuFrameworkS -> Quake Int

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

menuAdjustCursor :: Ref MenuFrameworkS -> Int -> Quake ()

menuDraw :: Ref MenuFrameworkS -> Quake ()

menuSlideItem :: Ref MenuFrameworkS -> Int -> Quake ()

menuSelectItem :: Ref MenuFrameworkS -> Quake Bool
