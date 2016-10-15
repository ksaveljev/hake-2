{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.MD2.DTriangleT
    ( module QCommon.QFiles.MD2.DTriangleT
    ) where

import           Control.Lens (makeLenses)
import           Data.Binary  (Get)

import           Types
import           Util.Binary  (getV3Int16)

makeLenses ''DTriangleT

getDTriangleT :: Get DTriangleT
getDTriangleT = DTriangleT <$> getV3Int16
                           <*> getV3Int16