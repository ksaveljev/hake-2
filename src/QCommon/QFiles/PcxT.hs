{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.PcxT
    ( module QCommon.QFiles.PcxT
    ) where

import           Control.Lens    (makeLenses)
import           Data.Binary.Get (Get, getWord16le, getByteString)

import           Types
import           Util.Binary     (getInt8)

makeLenses ''PcxT

getPcxT :: Int -> Get PcxT
getPcxT len = PcxT <$> getInt8
                   <*> getInt8
                   <*> getInt8
                   <*> getInt8
                   <*> getWord16le
                   <*> getWord16le
                   <*> getWord16le
                   <*> getWord16le
                   <*> getWord16le
                   <*> getWord16le
                   <*> getByteString 48
                   <*> getInt8
                   <*> getInt8
                   <*> getWord16le
                   <*> getWord16le
                   <*> getByteString 58
                   <*> getByteString (len - 128)