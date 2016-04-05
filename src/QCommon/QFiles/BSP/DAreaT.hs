{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.BSP.DAreaT
  ( module QCommon.QFiles.BSP.DAreaT
  ) where

import Types
import Util.Binary (getInt)

import Control.Lens (makeLenses)
import Data.Binary.Get (Get)

makeLenses ''DAreaT

dAreaTSize :: Int
dAreaTSize = 8

getDAreaT :: Get DAreaT
getDAreaT = DAreaT <$> getInt <*> getInt