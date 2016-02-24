module Game.Info
  ( printInfo
  , setValueForKey
  ) where

import           Types

import qualified Data.ByteString as B

printInfo :: B.ByteString -> Quake ()
printInfo = error "Info.printInfo" -- TODO

setValueForKey :: B.ByteString -> B.ByteString -> B.ByteString -> Quake B.ByteString
setValueForKey = error "Info.setValueForKey" -- TODO
