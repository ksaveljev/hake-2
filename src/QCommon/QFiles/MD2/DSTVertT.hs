{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.MD2.DSTVertT where

import Control.Applicative ((<*>))
import Control.Lens (makeLenses)
import Data.Functor ((<$>))
import Data.Int (Int16)
import qualified Data.ByteString.Lazy as BL

import Types
import Util.Binary

makeLenses ''DSTVertT

newDSTVertT :: BL.ByteString -> DSTVertT
newDSTVertT = runGet getDSTVertT

getDSTVertT :: Get DSTVertT
getDSTVertT = DSTVertT <$> getInt16
                       <*> getInt16
