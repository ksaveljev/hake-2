module QCommon.CVar 
  (get
  ,getExisting
  ,initialize
  ,update)
  where

import           Types

import qualified Data.ByteString as B

get :: B.ByteString -> B.ByteString -> Int -> Quake (Maybe CVarT)
get = undefined -- TODO

getExisting :: B.ByteString -> B.ByteString -> Int -> Quake CVarT
getExisting = undefined -- TODO

initialize :: Quake ()
initialize = undefined -- TODO

update :: CVarT -> Quake ()
update = undefined -- TODO
