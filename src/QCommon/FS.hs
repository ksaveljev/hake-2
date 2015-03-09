{-# LANGUAGE OverloadedStrings #-}
module QCommon.FS where

import Quake
import Internal
import qualified Game.Cmd as Cmd

initFileSystem :: Quake ()
initFileSystem = do
    Cmd.addCommand "path" pathF
    Cmd.addCommand "link" linkF
    Cmd.addCommand "dir" dirF

    undefined -- TODO

pathF :: XCommandT
pathF = undefined -- TODO

linkF :: XCommandT
linkF = undefined -- TODO

dirF :: XCommandT
dirF = undefined -- TODO
