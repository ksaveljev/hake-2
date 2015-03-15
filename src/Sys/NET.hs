module Sys.NET where

import qualified Data.ByteString as B

import Quake
import QCommon.NetAdrT

init :: Quake ()
init = return () -- nothing to do

-- Config multi or singlepalyer - A single player game will only use the loopback code.
config :: Bool -> Quake ()
config = undefined -- TODO

-- Creates an netadr_t from a string
stringToAdr :: B.ByteString -> Quake (Maybe NetAdrT)
stringToAdr = undefined -- TODO

-- Returns a string holding ip address and port like "ip0.ip1.ip2.ip3:port".
adrToString :: NetAdrT -> B.ByteString
adrToString = undefined -- TODO
