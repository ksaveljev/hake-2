{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.MD2.DAliasFrameT
    ( module QCommon.QFiles.MD2.DAliasFrameT
    ) where

import           Control.Lens        (makeLenses)
import           Data.Binary.Get     (Get, getByteString)
import qualified Data.ByteString     as B
import qualified Data.Vector.Unboxed as UV

import           Types
import           Util.Binary         (getInt, getV3Float)

makeLenses ''DAliasFrameT

getDAliasFrameT :: Int -> Get DAliasFrameT
getDAliasFrameT verticesCount = DAliasFrameT <$> getV3Float
                                             <*> getV3Float
                                             <*> (B.takeWhile (/= 0) <$> getByteString 16) -- trim name
                                             <*> UV.replicateM verticesCount getInt