module Game.Cmd where

import qualified Data.ByteString as B

import Quake
import QuakeState
import QCommon.XCommandT

addCommand :: B.ByteString -> Maybe XCommandT -> Quake ()

argc :: Quake Int

argv :: Int -> Quake B.ByteString

executeString :: B.ByteString -> Quake ()

clientCommand :: EdictReference -> Quake ()

helpF :: EdictReference -> Quake ()

removeCommand :: B.ByteString -> Quake ()
