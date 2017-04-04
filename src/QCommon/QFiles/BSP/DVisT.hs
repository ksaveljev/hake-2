{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.BSP.DVisT where

import           Control.Lens         (makeLenses)
import           Data.Binary.Get      (Get)
import qualified Data.ByteString.Lazy as BL
import           Data.Functor         ((<$>))
import qualified Data.Vector          as V

import           Types
import           Util.Binary

emptyDVisT :: DVisT
emptyDVisT = DVisT 0 V.empty

makeLenses ''DVisT

newDVisT :: BL.ByteString -> DVisT
newDVisT = runGet getDVisT
  where getDVisT :: Get DVisT
        getDVisT = do
          numClusters <- getInt
          DVisT numClusters <$> V.sequence (V.replicate numClusters getInt2)

getDVisT :: Get DVisT
getDVisT = do
    numClusters <- getInt
    DVisT numClusters <$> V.sequence (V.replicate numClusters getInt2)
