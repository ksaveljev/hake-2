module Sys.Socket ( Socket
                  , socket
                  , bind
                  , close
                  , sendTo
                  , recvFrom
                  ) where

import Control.Monad.Except
import Network.Socket (HostAddress, PortNumber)
import qualified Data.ByteString as B

data Socket = Socket Int

socket :: IO (Maybe Socket)
socket = liftIO (putStrLn "SOCKET STUB! DO NOT FORGET TO REPLACE ME WITH IMPLEMENTATION") >> undefined

bind :: Socket -> HostAddress -> PortNumber -> IO Bool
bind _ _ _ = liftIO (putStrLn "SOCKET STUB! DO NOT FORGET TO REPLACE ME WITH IMPLEMENTATION") >> undefined

close :: Socket -> IO ()
close _ = liftIO (putStrLn "SOCKET STUB! DO NOT FORGET TO REPLACE ME WITH IMPLEMENTATION") >> undefined

sendTo :: Socket -> B.ByteString -> HostAddress -> PortNumber -> IO Int
sendTo _ _ _ _ = liftIO (putStrLn "SOCKET STUB! DO NOT FORGET TO REPLACE ME WITH IMPLEMENTATION") >> undefined

recvFrom :: Socket -> Int -> IO (Maybe (B.ByteString, HostAddress, PortNumber))
recvFrom _ _ = liftIO (putStrLn "SOCKET STUB! DO NOT FORGET TO REPLACE ME WITH IMPLEMENTATION") >> undefined
