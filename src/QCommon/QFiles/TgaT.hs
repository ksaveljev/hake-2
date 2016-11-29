{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.TgaT
    ( module QCommon.QFiles.TgaT
    ) where

import           Control.Lens    (makeLenses)
import           Data.Binary.Get (Get, getWord8, getWord16le, getByteString)

import           Types

makeLenses ''TgaT

getTgaT :: Int -> Get TgaT
getTgaT len =
    TgaT <$> getWord8
         <*> getWord8
         <*> getWord8
         <*> getWord16le
         <*> getWord16le
         <*> getWord8
         <*> getWord16le
         <*> getWord16le
         <*> getWord16le
         <*> getWord16le
         <*> getWord8
         <*> getWord8
         <*> getByteString (len - 18)