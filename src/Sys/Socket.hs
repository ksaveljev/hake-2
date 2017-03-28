module Sys.Socket ( Socket
                  , socket
                  , bind
                  , close
                  , sendTo
                  , recvFrom
                  ) where

import Control.Monad.Except
import Network.Socket (HostAddress)
import qualified Data.ByteString as B

data Socket -- = Socket Int

socket :: IO (Maybe Socket)
socket = liftIO (putStrLn "SOCKET STUB! DO NOT FORGET TO REPLACE ME WITH IMPLEMENTATION") >> undefined

bind :: Num a => Socket -> HostAddress -> a -> IO Bool
bind _ _ _ = liftIO (putStrLn "SOCKET STUB! DO NOT FORGET TO REPLACE ME WITH IMPLEMENTATION") >> undefined

close :: Socket -> IO ()
close _ = liftIO (putStrLn "SOCKET STUB! DO NOT FORGET TO REPLACE ME WITH IMPLEMENTATION") >> undefined

sendTo :: Num a => Socket -> B.ByteString -> HostAddress -> a -> IO Int
sendTo _ _ _ _ = liftIO (putStrLn "SOCKET STUB! DO NOT FORGET TO REPLACE ME WITH IMPLEMENTATION") >> undefined

recvFrom :: Num a => Socket -> Int -> IO (Maybe (B.ByteString, HostAddress, a))
recvFrom _ _ = liftIO (putStrLn "SOCKET STUB! DO NOT FORGET TO REPLACE ME WITH IMPLEMENTATION") >> undefined
