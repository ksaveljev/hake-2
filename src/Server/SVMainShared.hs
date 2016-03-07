module Server.SVMainShared
  ( dropClient
  , shutdown
  ) where

import           Types

import qualified Data.ByteString as B

shutdown :: B.ByteString -> Bool -> Quake ()
shutdown = error "SVMain.shutdown" -- TODO

dropClient :: Ref ClientT -> Quake ()
dropClient = error "SVMain.dropClient" -- TODO