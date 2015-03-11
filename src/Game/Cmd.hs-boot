module Game.Cmd where

import qualified Data.ByteString as B

import Quake
import QCommon.XCommandT

addCommand :: B.ByteString -> Maybe XCommandT -> Quake ()
argc :: Quake Int
argv :: Int -> Quake B.ByteString
