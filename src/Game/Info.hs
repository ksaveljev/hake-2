module Game.Info
  ( printInfo
  ) where

import           Types

import qualified Data.ByteString as B

printInfo :: B.ByteString -> Quake ()
printInfo = error "Info.printInfo" -- TODO