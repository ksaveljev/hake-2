{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiWayIf #-}
module Game.GameBase where

import Control.Lens (use, (^.), (.=), Traversal', preuse)
import Data.Char (toLower)
import Data.Maybe (isNothing)
import Linear (V3(..))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Quake
import QuakeState

vecUp :: V3 Float
vecUp = V3 0 (-1) 0

moveDirUp :: V3 Float
moveDirUp = V3 0 0 1

vecDown :: V3 Float
vecDown = V3 0 (-2) 0

moveDirDown :: V3 Float
moveDirDown = V3 0 0 (-1)


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

setMoveDir :: Traversal' QuakeState (V3 Float) -> Traversal' QuakeState (V3 Float) -> Quake ()
setMoveDir anglesLens moveDirLens = do
    Just angles <- preuse anglesLens
    --Just movedir <- preuse moveDirLens

    if | angles == vecUp -> moveDirLens .= moveDirUp
       | angles == vecDown -> moveDirLens .= moveDirDown
       | otherwise -> io (putStrLn "GameBase.setMoveDir") >> undefined -- TODO

    anglesLens .= V3 0 0 0
