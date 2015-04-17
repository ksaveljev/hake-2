module Sys.IN where

import Quake

init :: Quake ()
init = io (putStrLn "IN.init") >> undefined -- TODO

shutdown :: Quake ()
shutdown = io (putStrLn "IN.shutdown") >> undefined -- TODO

realINInit :: Quake ()
realINInit = io (putStrLn "IN.realINInit") >> undefined -- TODO
