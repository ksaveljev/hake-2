{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.MD2.DTriangleT where

import Control.Applicative ((<*>))
import Control.Lens (makeLenses)
import Data.Functor ((<$>))
import Data.Int (Int16)
import Linear (V3)
import qualified Data.ByteString.Lazy as BL

import Util.Binary

data DTriangleT =
  DTriangleT { _dtIndexXYZ :: V3 Int16
             , _dtIndexST  :: V3 Int16
             }

makeLenses ''DTriangleT

newDTriangleT :: BL.ByteString -> DTriangleT
newDTriangleT = runGet getDTriangleT

getDTriangleT :: Get DTriangleT
getDTriangleT = DTriangleT <$> getV3Int16
                           <*> getV3Int16
