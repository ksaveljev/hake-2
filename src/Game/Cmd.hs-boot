module Game.Cmd where

import qualified Data.ByteString as B

import Quake

addCommand :: B.ByteString -> Quake () -> Quake ()
