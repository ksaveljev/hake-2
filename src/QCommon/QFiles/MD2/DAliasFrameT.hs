{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.MD2.DAliasFrameT where

import Control.Applicative ((<*>))
import Control.Lens (makeLenses)
import Data.Functor ((<$>))
import Linear (V3)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector.Unboxed as UV

import Util.Binary

data DAliasFrameT =
  DAliasFrameT { _dafScale     :: V3 Float
               , _dafTranslate :: V3 Float
               , _dafName      :: B.ByteString
               , _dafVerts     :: UV.Vector Int
               } deriving Eq

makeLenses ''DAliasFrameT

newDAliasFrameT :: BL.ByteString -> DAliasFrameT
newDAliasFrameT = runGet (getDAliasFrameT 0)

getDAliasFrameT :: Int -> Get DAliasFrameT
getDAliasFrameT verticesCount = DAliasFrameT <$> getV3Float
                                             <*> getV3Float
                                             <*> (B.takeWhile (/= 0) <$> getByteString 16) -- trim name
                                             <*> UV.replicateM verticesCount getInt
