{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.BSP.DVisT
  ( module QCommon.QFiles.BSP.DVisT
  ) where

import           Types

import           Control.Lens (makeLenses)
import qualified Data.Vector as V

makeLenses ''DVisT

emptyDVisT :: DVisT
emptyDVisT = DVisT 0 V.empty