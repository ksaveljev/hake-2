{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.BSP.DModelT where

import Control.Applicative ((<*>))
import Control.Lens (makeLenses)
import Data.Binary.Get
import Data.Binary.IEEE754 (getFloat32le)
import Data.Functor ((<$>))
import Linear (V3(..))
import qualified Data.ByteString.Lazy as BL

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

        getInt :: Get Int
        getInt = fromIntegral <$> getWord32le

        getV3Float :: Get (V3 Float)
        getV3Float = V3 <$> getFloat32le
                        <*> getFloat32le
                        <*> getFloat32le
