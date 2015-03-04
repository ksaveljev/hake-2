module QCommon.CVar where

-- CVar implements console variables. The original code is located in cvar.c

import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as UV

import Quake (Quake)
import Game.CVarT
import QCommon.XCommandT

get :: B.ByteString -> B.ByteString -> Int -> Quake CVarT
get = undefined -- TODO

init :: Quake ()
init = undefined -- TODO

variableString :: B.ByteString -> Quake B.ByteString
variableString = undefined -- TODO

findVar :: B.ByteString -> Quake CVarT
findVar = undefined -- TODO

fullSet :: B.ByteString -> B.ByteString -> B.ByteString -> Int -> Quake CVarT
fullSet = undefined -- TODO

set :: B.ByteString -> B.ByteString -> Quake CVarT
set = undefined -- TODO

forceSet :: B.ByteString -> B.ByteString -> Quake CVarT
forceSet = undefined -- TODO

set2 :: B.ByteString -> B.ByteString -> Bool -> Quake CVarT
set2 = undefined -- TODO

setF :: XCommandT
setF = undefined -- TODO

listF :: XCommandT
listF = undefined -- TODO

setValueI :: B.ByteString -> Int -> Quake ()
setValueI = undefined -- TODO

setValueF :: B.ByteString -> Float -> Quake ()
setValueF = undefined -- TODO

variableValue :: B.ByteString -> Quake Float
variableValue = undefined -- TODO

command :: Quake Bool
command = undefined -- TODO

bitInfo :: Int -> Quake B.ByteString
bitInfo = undefined -- TODO

serverInfo :: Quake B.ByteString
serverInfo = undefined -- TODO

getLatchedVars :: Quake ()
getLatchedVars = undefined -- TODO

userInfo :: Quake B.ByteString
userInfo = undefined -- TODO

writeVariables :: FilePath -> Quake ()
writeVariables = undefined -- TODO

completeVariable :: B.ByteString -> UV.Vector B.ByteString
completeVariable = undefined -- TODO

infoValidate :: B.ByteString -> Bool
infoValidate = undefined -- TODO
