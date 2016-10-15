{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.MD2.DSTVertT
    ( module QCommon.QFiles.MD2.DSTVertT
    ) where

import           Control.Lens (makeLenses)
import           Data.Binary  (Get)

import           Types
import           Util.Binary  (getInt16)

makeLenses ''DSTVertT

getDSTVertT :: Get DSTVertT
getDSTVertT = DSTVertT <$> getInt16
                       <*> getInt16