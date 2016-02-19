module Game.Info
  ( printServerInfo
  ) where

import           Types

import qualified Data.ByteString as B

printServerInfo :: B.ByteString -> Quake ()
printServerInfo = error "Info.printServerInfo" -- TODO