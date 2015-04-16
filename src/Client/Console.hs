{-# LANGUAGE OverloadedStrings #-}
module Client.Console where

import Control.Lens ((.=))
import Control.Monad (void)

import Quake
import QuakeState
import QCommon.XCommandT
import {-# SOURCE #-} qualified Game.Cmd as Cmd
import qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar

init :: Quake ()
init = do
    globals.con.cLineWidth .= -1

    checkResize

    Com.printf "Console initialized.\n"

    -- register our commands
    void $ CVar.get "con_notifytime" "3" 0

    Cmd.addCommand "toggleconsole" (Just toggleConsoleF)
    Cmd.addCommand "togglechat" (Just toggleChatF)
    Cmd.addCommand "messagemode" (Just messageModeF)
    Cmd.addCommand "messagemode2" (Just messageMode2F)
    Cmd.addCommand "clear" (Just clearF)
    Cmd.addCommand "condump" (Just dumpF)

    globals.con.cInitialized .= True

-- If the line width has changed, reformat the buffer.
checkResize :: Quake ()
checkResize = io (putStrLn "Console.checkResize") >> undefined -- TODO

toggleConsoleF :: XCommandT
toggleConsoleF = io (putStrLn "Console.toggleConsoleF") >> undefined -- TODO

toggleChatF :: XCommandT
toggleChatF = io (putStrLn "Console.toggleChatF") >> undefined -- TODO

messageModeF :: XCommandT
messageModeF = io (putStrLn "Console.messageModeF") >> undefined -- TODO

messageMode2F :: XCommandT
messageMode2F = io (putStrLn "Console.messageMode2F") >> undefined -- TODO

clearF :: XCommandT
clearF = io (putStrLn "Console.clearF") >> undefined -- TODO

dumpF :: XCommandT
dumpF = io (putStrLn "Console.dumpF") >> undefined -- TODO
