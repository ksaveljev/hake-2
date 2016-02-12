module QCommon.CVar 
  (get
  ,getExisting
  ,update)
  where

import           Types

import qualified Data.ByteString as B

get :: B.ByteString -> B.ByteString -> Int -> Quake (Maybe CVarT)
get = undefined -- TODO

getExisting :: B.ByteString -> B.ByteString -> Int -> Quake CVarT
getExisting = undefined -- TODO

update :: CVarT -> Quake ()
update = undefined -- TODO
