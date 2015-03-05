{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Game.Cmd where

import Data.Foldable (find)
import qualified Data.ByteString as B

import Quake
import qualified QCommon.CVar as CVar
import qualified QCommon.Com as Com

init :: Quake ()
init = do
    addCommand "exec" execF
    addCommand "echo" echoF
    addCommand "cmdlist" listF
    addCommand "alias" aliasF
    addCommand "wait" waitF

addCommand :: B.ByteString -> Quake () -> Quake ()
addCommand cmdName f = do
    varStr <- CVar.variableString cmdName
    cmdExists <- undefined --commandExists cmdName -- TODO

    -- fail if the command is a variable name
    -- fail if the command already exists
    -- add new command otherwise
    if | B.length varStr > 0 ->
           Com.printf $ "Cmd.addCommand: "
             `B.append` cmdName
             `B.append` " already defined as a var\n"
       | cmdExists -> undefined -- TODO
       | otherwise -> undefined -- TODO

execF :: Quake ()
execF = undefined -- TODO

echoF :: Quake ()
echoF = undefined -- TODO

listF :: Quake ()
listF = undefined -- TODO

aliasF :: Quake ()
aliasF = undefined -- TODO

waitF :: Quake ()
waitF = undefined -- TODO
