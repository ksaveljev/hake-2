{-# LANGUAGE TemplateHaskell #-}
module QCommon.MiptexT
    ( module QCommon.MiptexT
    ) where

import           Control.Lens        (makeLenses)
import           Data.Binary.Get     (Get, getByteString, skip)
import qualified Data.ByteString     as B
import qualified Data.Vector.Unboxed as UV

import           Types
import           Util.Binary         (getInt)

mipLevels :: Int
mipLevels = 4

miptexNameSize :: Int
miptexNameSize = 32

makeLenses ''MiptexT

getMiptexT :: Get MiptexT
getMiptexT = do
    name <- B.takeWhile (/= 0) <$> getByteString 32
    width <- getInt
    height <- getInt
    offsets <- getOffsets
    animFrame <- B.takeWhile (/= 0) <$> getByteString 32
    flags <- getInt
    contents <- getInt
    value <- getInt
    skip ((offsets UV.! 0) - 100) -- 100 bytes is header size
    buf <- getByteString (width * height)
    return (MiptexT name width height offsets animFrame flags contents value buf)
  where
    getOffsets = UV.replicateM 4 getInt
