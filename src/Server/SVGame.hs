module Server.SVGame where

import Quake
import qualified Game.GameBase as GameBase

{-
-  SV_ShutdownGameProgs
- 
- Called when either the entire server is being killed, or it is changing
- to a different game directory. 
-}
shutdownGameProgs :: Quake ()
shutdownGameProgs = GameBase.shutdownGame
