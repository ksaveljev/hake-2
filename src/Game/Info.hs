module Game.Info
    ( printInfo
    , setValueForKey
    , validate
    , valueForKey
    ) where

import qualified Data.ByteString         as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8   as BC
import qualified Data.ByteString.Lazy    as BL
import           Data.Monoid             ((<>))

import qualified Constants
import qualified QCommon.Com             as Com
import           Types

printInfo :: B.ByteString -> Quake ()
printInfo str =
    composePrintString tokens mempty >>= printString
  where
    splitStr = BC.split '\\' str
    tokens | null splitStr = splitStr
           | otherwise = tail splitStr -- ugly hack for BC.split because BC.split '\\' "\\cheats\\0" == ["", "cheats", "0"]
    printString Nothing = return ()
    printString (Just s) = Com.printf s

composePrintString :: [B.ByteString] -> BB.Builder -> Quake (Maybe B.ByteString)
composePrintString [] acc = return (Just (BL.toStrict (BB.toLazyByteString acc)))
composePrintString (k:v:xs) acc = composePrintString xs acc'
  where
    acc' = acc <> bs k <> bs spaces <> "=" <> bs v <> bs "\n"
    bs = BB.byteString
    spaces | klen < 20 = B.drop klen fillSpaces
           | otherwise = B.empty
    klen = B.length k
composePrintString _ _ = do
    Com.printf "MISSING VALUE\n"
    return Nothing

fillSpaces :: B.ByteString
fillSpaces = "                     "

setValueForKey :: B.ByteString -> B.ByteString -> B.ByteString -> Quake B.ByteString
setValueForKey str key value
    | B.null value = return str
    | BC.elem '\\' key || BC.elem '\\' value = do
        Com.printf "Can't use keys or values with a \\\n"
        return str
    | BC.elem ';' key = do
        Com.printf "Can't use keys or values with a semicolon\n"
        return str
    | BC.elem '"' key || BC.elem '"' value = do
        Com.printf "Can't use keys or values with a \"\n"
        return str
    | B.length key > Constants.maxInfoKey - 1 || B.length value > Constants.maxInfoKey - 1 = do
        Com.printf "Keys and values must be < 64 characters.\n"
        return str
    | otherwise = do
        strippedStr <- removeKey str key
        buildInfoString str strippedStr key value

buildInfoString :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString -> Quake B.ByteString
buildInfoString str strippedStr key value
    | B.length str + 2 + B.length key + B.length value > Constants.maxInfoString = do
        Com.printf "Info string length exceeded"
        return str
    | otherwise =
        return (B.concat [strippedStr, "\\", key, "\\", value])

removeKey :: B.ByteString -> B.ByteString -> Quake B.ByteString
removeKey str key
    | '\\' `BC.elem` key = do
        Com.printf "Can't use a key with a \\\n"
        return str
    | otherwise = composeTokens str key tokens mempty
  where
    splitStr = BC.split '\\' str
    tokens | null splitStr = splitStr
           | otherwise = tail splitStr -- ugly hack for BC.split because BC.split '\\' "\\cheats\\0" == ["", "cheats", "0"]

composeTokens :: B.ByteString -> B.ByteString -> [B.ByteString] -> BB.Builder -> Quake B.ByteString
composeTokens _ _ [] acc = return (BL.toStrict (BB.toLazyByteString acc))
composeTokens str key (k:v:xs) acc
    | k == key = composeTokens str key xs acc
    | otherwise = composeTokens str key xs (acc <> bslash <> bs k <> bslash <> bs v)
  where
    bs = BB.byteString
    bslash = bs "\\"
composeTokens str _ _ _ = do
    Com.printf "MISSING VALUE\n"
    return str

valueForKey :: B.ByteString -> B.ByteString -> Quake B.ByteString
valueForKey str key = findTokenValue str key (if null tokens then tokens else tail tokens)
  where
    tokens = BC.split '\\' str -- ulgy hack for BC.split because
                               -- BC.split '\\' "\\cheats\\0" == ["", "cheats", "0"]

findTokenValue :: B.ByteString -> B.ByteString -> [B.ByteString] -> Quake B.ByteString
findTokenValue _ _ [] = return B.empty
findTokenValue str key (k:v:xs)
    | k == key = return v
    | otherwise = findTokenValue str key xs
findTokenValue str _ _ = do
    Com.printf "MISSING VALUE\n"
    return str

validate :: B.ByteString -> Bool
validate str = not ('"' `BC.elem` str || ';' `BC.elem` str)