{-# LANGUAGE OverloadedStrings #-}
module Client.V where

import Control.Monad (void)

import Quake
import QCommon.XCommandT
import qualified Constants
import {-# SOURCE #-} qualified Game.Cmd as Cmd
import qualified QCommon.CVar as CVar

gunNextF :: XCommandT
gunNextF = io (putStrLn "V.gunNextF") >> undefined -- TODO

gunPrevF :: XCommandT
gunPrevF = io (putStrLn "V.gunPrevF") >> undefined -- TODO

gunModelF :: XCommandT
gunModelF = io (putStrLn "V.gunModelF") >> undefined -- TODO

viewPosF :: XCommandT
viewPosF = io (putStrLn "V.viewPosF") >> undefined -- TODO

init :: Quake ()
init = do
    Cmd.addCommand "gun_next" (Just gunNextF)
    Cmd.addCommand "gun_prev" (Just gunPrevF)
    Cmd.addCommand "gun_model" (Just gunModelF)

    Cmd.addCommand "viewpos" (Just viewPosF)

    void $ CVar.get "crosshair" "0" Constants.cvarArchive

    void $ CVar.get "cl_testblend" "0" 0
    void $ CVar.get "cl_testparticles" "0" 0
    void $ CVar.get "cl_testentities" "0" 0
    void $ CVar.get "cl_testlights" "0" 0

    void $ CVar.get "cl_stats" "0" 0
