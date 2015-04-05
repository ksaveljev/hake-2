{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.BSP.DModelT where

import Control.Applicative ((<*>))
import Control.Lens (makeLenses)
import Data.Functor ((<$>))
import Linear (V3)
import qualified Data.ByteString.Lazy as BL

import Util.Binary

dModelTSize :: Int
dModelTSize = 3 * 4 + 3 * 4 + 3 * 4 + 4 + 4 + 4

data DModelT =
  DModelT { _dmMins      :: V3 Float
          , _dmMaxs      :: V3 Float
          , _dmOrigin    :: V3 Float
          , _dmHeadNode  :: Int
          , _dmFirstFace :: Int
          , _dmNumFaces  :: Int
          }

makeLenses ''DModelT

newDModelT :: BL.ByteString -> DModelT
newDModelT = runGet getDModelT
  where getDModelT :: Get DModelT
        getDModelT = DModelT <$> getV3Float
                             <*> getV3Float
                             <*> getV3Float
                             <*> getInt
                             <*> getInt
                             <*> getInt
