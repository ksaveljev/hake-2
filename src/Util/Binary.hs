{-# LANGUAGE BangPatterns #-}
module Util.Binary
  ( encode
  , getInt
  , getInt8
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Data.Int (Int8, Int32)
import           Data.Binary.Get (Get, getWord32le, getWord8)

encode :: Show a => a -> B.ByteString
encode = BC.pack . show

getInt :: Get Int
getInt = let !x = fromIntegral <$> getWord32le :: Get Int32
         in fromIntegral <$> x
{-# INLINE getInt #-}

getInt8 :: Get Int8
getInt8 = fromIntegral <$> getWord8