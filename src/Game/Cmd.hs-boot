module Game.Cmd where

import qualified Data.ByteString as B

import Types
import QuakeState
import QCommon.XCommandT

addCommand :: B.ByteString -> Maybe XCommandT -> Quake ()

argc :: Quake Int

argv :: Int -> Quake B.ByteString

executeString :: B.ByteString -> Quake ()

clientCommand :: Ref EdictT -> Quake ()

helpF :: Ref EdictT -> Quake ()

removeCommand :: B.ByteString -> Quake ()
