{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.BSP.DFaceT where

import Control.Applicative ((<*>))
import Control.Lens (makeLenses)
import Data.Binary.Get
import Data.Functor ((<$>))
import Data.Int (Int16)
import Data.Word (Word16)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import qualified Constants

data DFaceT =
  DFaceT { _dfPlaneNum  :: Word16
         , _dfSide      :: Int16
         , _dfFirstEdge :: Int
         , _dfNumEdges  :: Int16
         , _dfTexInfo   :: Int16
         , _dfStyles    :: B.ByteString
         , _dfLightOfs  :: Int
         }

makeLenses ''DFaceT

newDFaceT :: BL.ByteString -> DFaceT
newDFaceT = runGet getDFaceT
  where getDFaceT :: Get DFaceT
        getDFaceT = DFaceT <$> getWord16le
                           <*> getInt16
                           <*> getInt
                           <*> getInt16
                           <*> getInt16
                           <*> getByteString Constants.maxLightMaps
                           <*> getInt

        getInt16 :: Get Int16
        getInt16 = fromIntegral <$> getWord16le

        getInt :: Get Int
        getInt = fromIntegral <$> getWord32le

