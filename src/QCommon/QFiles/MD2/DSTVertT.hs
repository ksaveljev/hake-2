{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.MD2.DSTVertT where

import Control.Applicative ((<*>))
import Control.Lens (makeLenses)
import Data.Binary.Get
import Data.Functor ((<$>))
import Data.Int (Int16)
import qualified Data.ByteString.Lazy as BL

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

        getInt16 :: Get Int16
        getInt16 = fromIntegral <$> getWord16le
