module Util.Binary
  ( encode
  , getInt
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Data.Int (Int32)
import           Data.Binary.Get (Get, getWord32le)

encode :: Show a => a -> B.ByteString
encode = BC.pack . show

getInt :: Get Int
getInt = let x = fromIntegral <$> getWord32le :: Get Int32
         in fromIntegral <$> x