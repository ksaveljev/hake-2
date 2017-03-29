module Client.Console where

import qualified Data.ByteString as B

import Types
import QCommon.XCommandT

init :: Quake ()

checkResize :: Quake ()

toggleConsoleF :: XCommandT

toggleChatF :: XCommandT

messageModeF :: XCommandT

messageMode2F :: XCommandT

clearF :: XCommandT

dumpF :: XCommandT

clearNotify :: Quake ()

drawAltString :: Int -> Int -> B.ByteString -> Quake ()

drawString :: Int -> Int -> B.ByteString -> Quake ()

drawConsole :: Float -> Quake ()

drawNotify :: Quake ()

print :: B.ByteString -> Quake ()
