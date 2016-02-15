module Game.Cmd
  (initialize)
  where

import           Types

import qualified Data.ByteString as B

initialCommands :: [(B.ByteString,XCommandT)]
initialCommands =
  [("exec",execF),("echo",echoF),("cmdlist",listF),("alias",aliasF),("wait",waitF)] 
  
initialize :: Quake ()
initialize = mapM_ (\(name,cmd) -> addCommand name (Just cmd)) initialCommands

addCommand :: B.ByteString -> Maybe XCommandT -> Quake ()
addCommand = error "Cmd.addCommand" -- TODO

execF :: XCommandT
execF = error "Cmd.execF" -- TODO

echoF :: XCommandT
echoF = error "Cmd.echoF" -- TODO

listF :: XCommandT
listF = error "Cmd.listF" -- TODO

aliasF :: XCommandT
aliasF = error "Cmd.aliasF" -- TODO

waitF :: XCommandT
waitF = error "Cmd.waitF" -- TODO