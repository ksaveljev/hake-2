{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.BSP.DVisT where

import Control.Applicative ((<*>))
import Control.Lens (makeLenses)
import Data.Binary.Get
import Data.Functor ((<$>))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

data DVisT =
  DVisT { _dvNumClusters :: Int
        , _dvBitOfs      :: V.Vector (Int, Int)
        }

makeLenses ''DVisT

newDVisT :: BL.ByteString -> DVisT
newDVisT = runGet getDVisT
  where getDVisT :: Get DVisT
        getDVisT = do
          numClusters <- getInt
          DVisT numClusters <$> V.sequence (V.replicate numClusters getInt2)

        getInt :: Get Int
        getInt = fromIntegral <$> getWord32le

        getInt2 :: Get (Int, Int)
        getInt2 = (,) <$> getInt <*> getInt