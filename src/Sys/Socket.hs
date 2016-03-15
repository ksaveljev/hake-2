module Sys.Socket
  ( close
  ) where

import Types

close :: Socket -> IO ()
close _ = io (putStrLn "SOCKET STUB! DO NOT FORGET TO REPLACE ME WITH IMPLEMENTATION!")
