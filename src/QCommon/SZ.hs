{-# LANGUAGE Rank2Types #-}
module QCommon.SZ
  (initialize
  ,write)
  where

import           Types

import           Control.Lens (Traversal', (.=))
import qualified Data.ByteString as B

initialize :: Traversal' QuakeState SizeBufT -> B.ByteString -> Int -> Quake ()
initialize bufLens bufData maxLen =
  bufLens .= SizeBufT False False bufData maxLen 0 0

write :: Traversal' QuakeState SizeBufT -> B.ByteString -> Int -> Quake ()
write = error "SZ.write"