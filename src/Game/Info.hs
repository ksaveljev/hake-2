{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Game.Info where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Quake
import qualified Constants
import qualified QCommon.Com as Com

print :: B.ByteString -> Quake ()
print = undefined -- TODO

-- Sets a value for a key in the user info string.
setValueForKey :: B.ByteString -> B.ByteString -> B.ByteString -> Quake B.ByteString
setValueForKey str key value = do
    if | B.length value == 0 -> return str
       | BC.elem '\\' key || BC.elem '\\' value -> do
           Com.printf "Can't use keys or values with a \\\n"
           return str
       | BC.elem ';' key -> do
           Com.printf "Can't use keys or values with a semicolon\n"
           return str
       | BC.elem '"' key || BC.elem '"' value -> do
           Com.printf "Can't use keys or values with a \"\n"
           return str
       | B.length key > Constants.maxInfoKey - 1 || B.length value > Constants.maxInfoKey - 1 -> do
           Com.printf "Keys and values must be < 64 characters.\n"
           return str
       | otherwise -> do
           undefined -- TODO
