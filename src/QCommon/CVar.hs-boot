module QCommon.CVar 
    ( command
    , findVar
    , forceSet
    , fullSet
    , get
    , getLatchedVars
    , initialize
    , initializeCVars
    , serverInfo
    , set
    , setValueF
    , setValueI
    , update
    , userInfo
    , variableString
    , variableValue
    , writeVariables
    ) where

import qualified Data.ByteString    as B

import           Types

command :: Quake Bool

findVar :: B.ByteString -> Quake (Maybe CVarT)

forceSet :: B.ByteString -> B.ByteString -> Quake CVarT

fullSet :: B.ByteString -> B.ByteString -> Int -> Quake CVarT

get :: B.ByteString -> B.ByteString -> Int -> Quake (Maybe CVarT)

getLatchedVars :: Quake ()

initialize :: Quake ()

initializeCVars :: [(B.ByteString, B.ByteString, Int)] -> Quake ()

serverInfo :: Quake B.ByteString

set :: B.ByteString -> B.ByteString -> Quake CVarT

setValueF :: B.ByteString -> Float -> Quake ()

setValueI :: B.ByteString -> Int -> Quake ()

update :: CVarT -> Quake ()

userInfo :: Quake B.ByteString

variableString :: B.ByteString -> Quake B.ByteString

variableValue :: B.ByteString -> Quake Float

writeVariables :: B.ByteString -> Quake ()
