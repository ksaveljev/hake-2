{-# LANGUAGE OverloadedStrings #-}
module Game.GameBase where

import Control.Lens (use, (^.))

import Quake
import QuakeState

shutdownGame :: Quake ()
shutdownGame = do
    gameimport <- use $ gameBaseGlobals.gbGameImport
    (gameimport^.giDprintf) "==== ShutdownGame ====\n"

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
