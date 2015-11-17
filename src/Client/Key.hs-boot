module Client.Key where

import System.IO (Handle)
import qualified Data.ByteString as B

import Quake
import QCommon.XCommandT

init :: Quake ()

bindF :: XCommandT

unbindF :: XCommandT

unbindAllF :: XCommandT

bindListF :: XCommandT

stringToKeynum :: B.ByteString -> Quake Int

keynumToString :: Int -> Quake B.ByteString

setBinding :: Int -> Maybe B.ByteString -> Quake ()

writeBindings :: Handle -> Quake ()

event :: Int -> Bool -> Int -> Quake ()

message :: Int -> Quake ()

console :: Int -> Quake ()

clearStates :: Quake ()
