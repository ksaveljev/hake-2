module QCommon.CVar where

import qualified Data.ByteString as B
import qualified Data.Vector as V

import Types
import QuakeState
import QCommon.XCommandT

get :: B.ByteString -> B.ByteString -> Int -> Quake (Maybe CVarT)

update :: CVarT -> Quake ()

initialize :: Quake ()

variableString :: B.ByteString -> Quake B.ByteString

findVar :: B.ByteString -> Quake (Maybe CVarT)

getExisting :: B.ByteString -> Quake CVarT

fullSet :: B.ByteString -> B.ByteString -> Int -> Quake CVarT

set :: B.ByteString -> B.ByteString -> Quake CVarT

forceSet :: B.ByteString -> B.ByteString -> Quake CVarT

set2 :: B.ByteString -> B.ByteString -> Bool -> Quake CVarT

setF :: XCommandT

listF :: XCommandT

setValueI :: B.ByteString -> Int -> Quake ()

setValueF :: B.ByteString -> Float -> Quake ()

variableValue :: B.ByteString -> Quake Float

command :: Quake Bool

bitInfo :: Int -> Quake B.ByteString

serverInfo :: Quake B.ByteString

getLatchedVars :: Quake ()

userInfo :: Quake B.ByteString

writeVariables :: B.ByteString -> Quake ()

completeVariable :: B.ByteString -> Quake (V.Vector B.ByteString)

infoValidate :: B.ByteString -> Bool
