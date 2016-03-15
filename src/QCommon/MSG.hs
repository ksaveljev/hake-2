{-# LANGUAGE Rank2Types #-}
module QCommon.MSG
  ( writeByteI
  , writeString
  ) where

import qualified QCommon.SZ as SZ
import           Types

import           Control.Lens (Traversal')
import qualified Data.ByteString as B

writeByteI :: Traversal' QuakeState SizeBufT -> Int -> Quake ()
writeByteI = error "MSG.writeByteI" -- TODO

writeString :: Traversal' QuakeState SizeBufT -> B.ByteString -> Quake ()
writeString sizeBufLens str = do
  SZ.write sizeBufLens str (B.length str)
  writeByteI sizeBufLens 0
