module Sys.NET where

import Quake

init :: Quake ()
init = return () -- nothing to do

-- Config multi or singlepalyer - A single player game will only use the loopback code.
config :: Bool -> Quake ()
config = undefined -- TODO
