module Util.Lib where

import Data.Int (Int16)
import Linear (V3)
import System.IO (Handle, IOMode)
import qualified Data.ByteString as B

import Quake

atof :: B.ByteString -> Float

atoi :: B.ByteString -> Int

atov :: B.ByteString -> V3 Float

vtos :: V3 Float -> B.ByteString

fOpen :: B.ByteString -> IOMode -> Quake (Maybe Handle)

fClose :: Handle -> Quake ()

rand :: Quake Int16

randomF :: Quake Float

crandom :: Quake Float
