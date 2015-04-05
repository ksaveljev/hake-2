{-# LANGUAGE ScopedTypeVariables #-}
module Util.Binary ( module Util.Binary
                   , module Data.Binary.Get
                   , module Data.Binary.IEEE754
                   ) where

import Data.Binary.Get
import Data.Binary.IEEE754 (getFloat32le)
import Data.Functor ((<$>))
import Data.Int
import Control.Applicative ((<*>))
import Linear (V3(..), V4(..))

getInt :: Get Int
getInt = let x :: Get Int32 = fromIntegral <$> getWord32le
         in fromIntegral <$> x

getInt8 :: Get Int8
getInt8 = fromIntegral <$> getWord8

getInt16 :: Get Int16
getInt16 = fromIntegral <$> getWord16le

getV3Int16 :: Get (V3 Int16)
getV3Int16 = V3 <$> getInt16
                <*> getInt16
                <*> getInt16

getV3Float :: Get (V3 Float)
getV3Float = V3 <$> getFloat32le
                <*> getFloat32le
                <*> getFloat32le

getV4Float :: Get (V4 Float)
getV4Float = V4 <$> getFloat32le
                <*> getFloat32le
                <*> getFloat32le
                <*> getFloat32le

getInt2 :: Get (Int, Int)
getInt2 = (,) <$> getInt <*> getInt

getInt4 :: Get (Int, Int, Int, Int)
getInt4 = (,,,) <$> getInt <*> getInt <*> getInt <*> getInt
