module QCommon.CVar ( get
                    ) where

-- CVar implements console variables. The original code is located in cvar.c

import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as UV

import Quake (Quake)
import Game.CVarT
import QCommon.XCommandT

get :: B.ByteString -> B.ByteString -> Int -> Quake CVarT
get = undefined

init :: Quake ()
init = undefined

variableString :: B.ByteString -> Quake B.ByteString
variableString = undefined

findVar :: B.ByteString -> Quake CVarT
findVar = undefined

fullSet :: B.ByteString -> B.ByteString -> B.ByteString -> Int -> Quake CVarT
fullSet = undefined

set :: B.ByteString -> B.ByteString -> Quake CVarT
set = undefined

forceSet :: B.ByteString -> B.ByteString -> Quake CVarT
forceSet = undefined

set2 :: B.ByteString -> B.ByteString -> Bool -> Quake CVarT
set2 = undefined

setF :: XCommandT
setF = undefined

listF :: XCommandT
listF = undefined

setValueI :: B.ByteString -> Int -> Quake ()
setValueI = undefined

setValueF :: B.ByteString -> Float -> Quake ()
setValueF = undefined

variableValue :: B.ByteString -> Quake Float
variableValue = undefined

command :: Quake Bool
command = undefined

bitInfo :: Int -> Quake B.ByteString
bitInfo = undefined

serverInfo :: Quake B.ByteString
serverInfo = undefined

getLatchedVars :: Quake ()
getLatchedVars = undefined

userInfo :: Quake B.ByteString
userInfo = undefined

writeVariables :: FilePath -> Quake ()
writeVariables = undefined

completeVariable :: B.ByteString -> UV.Vector B.ByteString
completeVariable = undefined

infoValidate :: B.ByteString -> Bool
infoValidate = undefined
