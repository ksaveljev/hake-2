module QCommon.MD4 where

import Control.Applicative ((<*>))
import Crypto.Hash.MD4 (hashlazy)
import Data.Binary.Get
import Data.Bits (xor)
import Data.Functor ((<$>))
import Data.Int (Int64)
import qualified Data.ByteString.Lazy as BL

blockChecksum :: BL.ByteString -> Int64 -> Int
blockChecksum buffer len =
    let hashedBuffer = BL.fromStrict $ hashlazy (BL.take len buffer)
        getInt = fromIntegral <$> getWord32le
        getInt4 = (,,,) <$> getInt <*> getInt <*> getInt <*> getInt
        (a, b, c, d) = runGet getInt4 hashedBuffer
    in a `xor` b `xor` c `xor` d
