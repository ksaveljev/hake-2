module Game.Info
  ( printInfo
  , setValueForKey
  ) where

import qualified Constants
import qualified QCommon.Com as Com
import           Types

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

printInfo :: B.ByteString -> Quake ()
printInfo = error "Info.printInfo" -- TODO

setValueForKey :: B.ByteString -> B.ByteString -> B.ByteString -> Quake B.ByteString
setValueForKey str key value
  | B.null value = return str
  | BC.elem '\\' key || BC.elem '\\' value =
      do Com.printf "Can't use keys or values with a \\\n"
         return str
  | BC.elem ';' key =
      do Com.printf "Can't use keys or values with a semicolon\n"
         return str
  | BC.elem '"' key || BC.elem '"' value =
      do Com.printf "Can't use keys or values with a \"\n"
         return str
  | B.length key > Constants.maxInfoKey - 1 || B.length value > Constants.maxInfoKey - 1 =
      do Com.printf "Keys and values must be < 64 characters.\n"
         return str
  | otherwise =
      do strippedStr <- removeKey str key
         buildInfoString str strippedStr key value

buildInfoString :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString -> Quake B.ByteString
buildInfoString str strippedStr key value
  | B.length str + 2 + B.length key + B.length value > Constants.maxInfoString =
      do Com.printf "Info string length exceeded"
         return str
  | otherwise =
      return (B.concat [strippedStr, "\\", key, "\\", value])

removeKey :: B.ByteString -> B.ByteString -> Quake B.ByteString
removeKey = error "Info.removeKey" -- TODO
