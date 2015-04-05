{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.MD2.DSTVertT where

import Control.Applicative ((<*>))
import Control.Lens (makeLenses)
import Data.Functor ((<$>))
import Data.Int (Int16)
import qualified Data.ByteString.Lazy as BL

import Util.Binary

data DSTVertT =
  DSTVertT { _dstvS :: Int16
           , _dstvT :: Int16
           }

makeLenses ''DSTVertT

newDSTVertT :: BL.ByteString -> DSTVertT
newDSTVertT = runGet getDSTVertT
  where getDSTVertT :: Get DSTVertT
        getDSTVertT = DSTVertT <$> getInt16
                               <*> getInt16
