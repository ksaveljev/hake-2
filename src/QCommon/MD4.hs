module QCommon.MD4 where

import Crypto.Hash.MD4 (hashlazy)
import Data.Bits (xor)
import Data.Int (Int64)
import qualified Data.ByteString.Lazy as BL

import Util.Binary

blockChecksum :: BL.ByteString -> Int64 -> Int
blockChecksum buffer len =
    let hashedBuffer = BL.fromStrict $ hashlazy (BL.take len buffer)
        (a, b, c, d) = runGet getInt4 hashedBuffer
    in a `xor` b `xor` c `xor` d
