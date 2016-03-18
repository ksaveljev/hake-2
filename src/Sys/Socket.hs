module Sys.Socket
  ( close
  , recvFrom
  ) where

import           Types

import qualified Data.ByteString as B
import           Network.Socket (HostAddress)

close :: Socket -> IO ()
close _ = io (putStrLn "SOCKET STUB! DO NOT FORGET TO REPLACE ME WITH IMPLEMENTATION!") -- TODO

recvFrom :: Num a => Socket -> Int -> IO (Maybe (B.ByteString, HostAddress, a))
recvFrom _ _ = io (putStrLn "SOCKET STUB! DO NOT FORGET TO REPLACE ME WITH IMPLEMENTATION") >> error "Socket.recvFrom" -- TODO