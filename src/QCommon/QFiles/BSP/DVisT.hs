{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.BSP.DVisT
  ( module QCommon.QFiles.BSP.DVisT
  ) where

import           Types
import           Util.Binary (getInt, getInt2)

import           Control.Lens (makeLenses)
import           Data.Binary.Get (Get)
import qualified Data.Vector as V

makeLenses ''DVisT

getDVisT :: Get DVisT
getDVisT =
  do numClusters <- getInt
     DVisT numClusters <$> V.sequence (V.replicate numClusters getInt2)

emptyDVisT :: DVisT
emptyDVisT = DVisT 0 V.empty