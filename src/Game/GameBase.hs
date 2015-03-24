module Game.GameBase where

import Quake
import {-# SOURCE #-} Game.GameImportT

shutdownGame :: Quake ()
shutdownGame = io (putStrLn "GameBase.shutdownGame") >> undefined -- TODO

{-
- G_RunFrame
-  
- Advances the world by Defines.FRAMETIME (0.1) seconds.
-}
runFrame :: Quake ()
runFrame = io (putStrLn "GameBase.runFrame") >> undefined -- TODO

{-
- This return a pointer to the structure with all entry points and global
- variables. 
-}
getGameApi :: GameImportT -> Quake ()
getGameApi _ = io (putStrLn "GameBase.getGameApi") >> undefined -- TODO
