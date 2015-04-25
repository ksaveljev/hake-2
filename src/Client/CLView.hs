module Client.CLView where

import Quake

prepRefresh :: Quake ()
prepRefresh = io (putStrLn "CLView.prepRefresh") >> undefined -- TODO
