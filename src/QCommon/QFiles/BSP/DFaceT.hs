{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.BSP.DFaceT
  ( module QCommon.QFiles.BSP.DFaceT
  ) where

import qualified Constants
import           Types
import           Util.Binary (getInt, getInt16)

import           Control.Lens (makeLenses)
import           Data.Binary.Get (Get, getWord16le, getByteString)

makeLenses ''DFaceT

dFaceTSize :: Int
dFaceTSize = 4 * Constants.sizeOfShort + 2 * Constants.sizeOfInt + Constants.maxLightMaps

getDFaceT :: Get DFaceT
getDFaceT = DFaceT <$> getWord16le
                   <*> getInt16
                   <*> getInt
                   <*> getInt16
                   <*> getInt16
                   <*> getByteString Constants.maxLightMaps
                   <*> getInt
