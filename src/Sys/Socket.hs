module Sys.Socket
    ( close
    , recvFrom
    , sendTo
    ) where

import qualified Data.ByteString as B
import           Network.Socket  (HostAddress)

import           Types

close :: Socket -> IO ()
close _ = io (putStrLn "SOCKET STUB! DO NOT FORGET TO REPLACE ME WITH IMPLEMENTATION!") -- TODO

recvFrom :: Num a => Socket -> Int -> IO (Maybe (B.ByteString, HostAddress, a))
recvFrom _ _ = io (putStrLn "SOCKET STUB! DO NOT FORGET TO REPLACE ME WITH IMPLEMENTATION") >> error "Socket.recvFrom" -- TODO

sendTo :: Num a => Socket -> B.ByteString -> HostAddress -> a -> IO Int
sendTo _ _ _ _ = io (putStrLn "SOCKET STUB! DO NOT FORGET TO REPLACE ME WITH IMPLEMENTATION") >> error "Socket.sendTo" -- TODO