{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.BSP.DModelT where

import Control.Applicative ((<*>))
import Control.Lens (makeLenses)
import Data.Functor ((<$>))
import Linear (V3)
import qualified Data.ByteString.Lazy as BL

import Types
import Util.Binary

dModelTSize :: Int
dModelTSize = 3 * 4 + 3 * 4 + 3 * 4 + 4 + 4 + 4

makeLenses ''DModelT

newDModelT :: BL.ByteString -> DModelT
newDModelT = runGet getDModelT

getDModelT :: Get DModelT
getDModelT = DModelT <$> getV3Float
                     <*> getV3Float
                     <*> getV3Float
                     <*> getInt
                     <*> getInt
                     <*> getInt
