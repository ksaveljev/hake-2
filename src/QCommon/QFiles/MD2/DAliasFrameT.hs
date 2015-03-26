{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.MD2.DAliasFrameT where

import Control.Applicative ((<*>))
import Control.Lens (makeLenses)
import Data.Binary.Get
import Data.Binary.IEEE754 (getFloat32le)
import Data.Functor ((<$>))
import Linear (V3(..))
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector.Unboxed as UV

data DAliasFrameT =
  DAliasFrameT { _dafScale     :: V3 Float
               , _dafTranslate :: V3 Float
               , _dafName      :: B.ByteString
               , _dafVerts     :: UV.Vector Int
               }

makeLenses ''DAliasFrameT

newDAliasFrameT :: BL.ByteString -> DAliasFrameT
newDAliasFrameT = runGet getDAliasFrameT
  where getDAliasFrameT :: Get DAliasFrameT
        getDAliasFrameT = DAliasFrameT <$> getV3Float
                                       <*> getV3Float
                                       <*> (B.takeWhile (/= 0) <$> getByteString 16) -- trim name
                                       <*> getEmptyVector

        getV3Float :: Get (V3 Float)
        getV3Float = V3 <$> getFloat32le
                        <*> getFloat32le
                        <*> getFloat32le

        getEmptyVector :: Get (UV.Vector Int)
        getEmptyVector = return UV.empty
