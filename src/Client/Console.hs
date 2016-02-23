module Client.Console
  ( initialize
  ) where

import qualified Game.Cmd as Cmd
import qualified QCommon.CVar as CVar
import qualified QCommon.Com as Com
import           QuakeState
import           Types

import           Control.Lens ((.=))
import           Control.Monad (void)
import qualified Data.ByteString as B

initialCommands :: [(B.ByteString, Maybe XCommandT)]
initialCommands =
  [ ("toggleconsole", Just toggleConsoleF), ("togglechat", Just toggleChatF)
  , ("messagemode", Just messageModeF), ("messagemode2", Just messageMode2F)
  , ("clear", Just clearF), ("condump", Just dumpF) ]

initialize :: Quake ()
initialize =
  do globals.gCon.cLineWidth .= -1
     checkResize
     Com.printf "Console initialized.\n"
     void (CVar.get "con_notifytime" "3" 0)
     Cmd.addInitialCommands initialCommands
     globals.gCon.cInitialized .= True

checkResize :: Quake ()
checkResize = error "Console.checkResize" -- TODO

toggleConsoleF :: XCommandT
toggleConsoleF = XCommandT "Console.toggleConsoleF" $
  error "Console.toggleConsoleF" -- TODO

toggleChatF :: XCommandT
toggleChatF = XCommandT "Console.toggleChatF" $
  error "Console.toggleChatF" -- TODO

messageModeF :: XCommandT
messageModeF = XCommandT "Console.messageModeF" $
  error "Console.messageModeF" -- TODO

messageMode2F :: XCommandT
messageMode2F = XCommandT "Console.messageMode2F" $
  error "Console.messageMode2F" -- TODO

clearF :: XCommandT
clearF = XCommandT "Console.clearF" $
  error "Console.clearF" -- TODO

dumpF :: XCommandT
dumpF = XCommandT "Console.dumpF" $
  error "Console.dumpF" -- TODO