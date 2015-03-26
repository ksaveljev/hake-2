{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.MD2.DTriangleT where

import Control.Applicative ((<*>))
import Control.Lens (makeLenses)
import Data.Binary.Get
import Data.Functor ((<$>))
import Data.Int (Int16)
import Linear (V3(..))
import qualified Data.ByteString.Lazy as BL

data DTriangleT =
  DTriangleT { _dtIndexXYZ :: V3 Int16
             , _dtIndexST  :: V3 Int16
             }

makeLenses ''DTriangleT

newDTriangleT :: BL.ByteString -> DTriangleT
newDTriangleT = runGet getDTriangleT
  where getDTriangleT :: Get DTriangleT
        getDTriangleT = DTriangleT <$> getV3Int16
                                   <*> getV3Int16

        getV3Int16 :: Get (V3 Int16)
        getV3Int16 = V3 <$> getInt16
                        <*> getInt16
                        <*> getInt16

        getInt16 :: Get Int16
        getInt16 = fromIntegral <$> getWord16le
