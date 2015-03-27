{-# LANGUAGE TemplateHaskell #-}
module QCommon.TexInfoT where

import Control.Applicative ((<*>))
import Control.Lens (makeLenses)
import Data.Binary.Get
import Data.Binary.IEEE754 (getFloat32le)
import Data.Functor ((<$>))
import Linear.V4 (V4(..))
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

texInfoTSize :: Int
texInfoTSize = 32 + 4 + 4 + 32 + 4

data TexInfoT =
  TexInfoT { _tiVecs        :: (V4 Float, V4 Float)
           , _tiFlags       :: Int
           , _tiValue       :: Int
           , _tiTexture     :: B.ByteString
           , _tiNextTexInfo :: Int
           }

makeLenses ''TexInfoT

newTexInfoT :: BL.ByteString -> TexInfoT
newTexInfoT = runGet getTexInfoT
  where getTexInfoT :: Get TexInfoT
        getTexInfoT = TexInfoT <$> ((,) <$> getV4Float <*> getV4Float)
                               <*> getInt
                               <*> getInt
                               <*> (B.takeWhile (/= 0) <$> getByteString 32)
                               <*> getInt

        getInt :: Get Int
        getInt = fromIntegral <$> getWord32le

        getV4Float :: Get (V4 Float)
        getV4Float = V4 <$> getFloat32le
                        <*> getFloat32le
                        <*> getFloat32le
                        <*> getFloat32le
