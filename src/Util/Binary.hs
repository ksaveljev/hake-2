{-# LANGUAGE ScopedTypeVariables #-}
module Util.Binary ( module Util.Binary
                   , module Data.Binary.Get
                   , module Data.Binary.IEEE754
                   ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Binary.Get
import Data.Binary.Put
import Data.Binary.IEEE754
import Data.Functor ((<$>))
import Data.Int
import Data.Word (Word16)
import Control.Applicative ((<*>))
import Linear (V3(..), V4(..))

encode :: Show a => a -> B.ByteString
encode = BC.pack . show

getInt :: Get Int
getInt = let x :: Get Int32 = fromIntegral <$> getWord32le
         in fromIntegral <$> x

putInt :: Int -> Put
putInt = putWord32le <$> fromIntegral

getFloat :: Get Float
getFloat = getFloat32le

putFloat :: Float -> Put
putFloat = putFloat32le

getBool :: Get Bool
getBool = let x = getWord8
          in (\v -> if v == 1 then True else False) <$> x

putBool :: Bool -> Put
putBool True = putWord8 1
putBool False = putWord8 0

getInt8 :: Get Int8
getInt8 = fromIntegral <$> getWord8

getInt16 :: Get Int16
getInt16 = fromIntegral <$> getWord16le

putInt16 :: Int16 -> Put
putInt16 = putWord16le <$> fromIntegral

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

getWord162 :: Get (Word16, Word16)
getWord162 = (,) <$> getWord16le <*> getWord16le

getInt4 :: Get (Int, Int, Int, Int)
getInt4 = (,,,) <$> getInt <*> getInt <*> getInt <*> getInt
