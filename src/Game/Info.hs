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
           strippedStr <- removeKey str key

           if B.length strippedStr + 2 + B.length key + B.length value > Constants.maxInfoString
             then do
               Com.printf "Info string length exceeded\n"
               return str
             else
               return $ strippedStr `B.append` "\\" `B.append` key `B.append` "\\" `B.append` value

removeKey :: B.ByteString -> B.ByteString -> Quake B.ByteString
removeKey str key = do
    if BC.elem '\\' key
      then do
        Com.printf "Can't use a key with a \\\n"
        return str
      else do
        let tokens = BC.split '\\' str
        composeTokens tokens ""

  where composeTokens :: [B.ByteString] -> B.ByteString -> Quake B.ByteString
        composeTokens [] acc = return acc
        composeTokens (a:b:xs) acc = do
          if a == key
            then composeTokens xs acc
            else composeTokens xs (acc `B.append` "\\" `B.append` a `B.append` "\\" `B.append` b)
        composeTokens _ _ = do
          Com.printf "MISSING VALUE\n"
          return str
