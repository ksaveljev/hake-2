{-# LANGUAGE OverloadedStrings #-}
module Game.GameBase where

import Control.Lens (use, (^.), (.=))
import Data.Char (toLower)
import Data.Maybe (isNothing)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

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
getGameApi imp =
    gameBaseGlobals.gbGameImport .= imp
    {- TODO:
        gi.pointcontents = new pmove_t.PointContentsAdapter() {
            public int pointcontents(float[] o) {
                return SV_WORLD.SV_PointContents(o);
            }
        };
    -} 

findByTarget :: EdictT -> B.ByteString -> Bool
findByTarget e s =
    if isNothing (e^.eEdictInfo.eiTargetName)
      then False
      else let Just targetName = e^.eEdictInfo.eiTargetName
           in BC.map toLower targetName == BC.map toLower s

gFind :: Maybe EdictReference -> (EdictT -> B.ByteString -> Bool) -> B.ByteString -> Quake (Maybe EdictReference)
gFind _ _ _ = io (putStrLn "GameBase.gFind") >> undefined -- TODO
