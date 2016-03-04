{-# LANGUAGE TemplateHaskell #-}
module QCommon.PackFileT
  ( module QCommon.PackFileT
  ) where

import           Types
import           Util.Binary (getInt)

import           Control.Lens (makeLenses)
import           Data.Binary.Get (Get, getByteString)
import qualified Data.ByteString as B

makeLenses ''PackFileT

packFileSize :: Int
packFileSize = 64

packFileNameSize :: Int
packFileNameSize = 56

getPackFile :: Get PackFileT
getPackFile = PackFileT <$> getName <*> getInt <*> getInt
  where getName = strip <$> getByteString packFileNameSize
        strip = B.takeWhile (/= 0)