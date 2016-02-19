module Server.SVMainShared
  ( shutdown
  ) where

import           Types

import qualified Data.ByteString as B

shutdown :: B.ByteString -> Bool -> Quake ()
shutdown = error "SVMain.shutdown" -- TODO