{-# LANGUAGE TemplateHaskell #-}
module QCommon.PackFileT
    ( module QCommon.PackFileT
    ) where

import           Control.Lens    (makeLenses)
import           Data.Binary.Get (Get, getByteString)
import qualified Data.ByteString as B

import           Types
import           Util.Binary     (getInt)

makeLenses ''PackFileT

packFileSize :: Int
packFileSize = 64

packFileNameSize :: Int
packFileNameSize = 56

getPackFile :: Get PackFileT
getPackFile = PackFileT <$> getName <*> getInt <*> getInt
  where
    getName = strip <$> getByteString packFileNameSize
    strip str = maybe str (`B.take` str) (B.findIndex (== 0) str)