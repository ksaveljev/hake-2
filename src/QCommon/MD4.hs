module QCommon.MD4 where

import           Util.Binary (getInt4T)

import           Crypto.Hash.MD4 (hashlazy)
import           Data.Binary.Get (runGet)
import           Data.Bits (xor)
import qualified Data.ByteString.Lazy as BL
import           Data.Int (Int64)

blockChecksum :: BL.ByteString -> Int64 -> Int
blockChecksum buffer len =
  let hashedBuffer = BL.fromStrict (hashlazy (BL.take len buffer))
      (a, b, c, d) = runGet getInt4T hashedBuffer
  in a `xor` b `xor` c `xor` d