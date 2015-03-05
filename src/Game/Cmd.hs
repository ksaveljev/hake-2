{-# LANGUAGE OverloadedStrings #-}
module Game.Cmd where

import qualified Data.ByteString as B

import Quake

init :: Quake ()
init = do
    addCommand "exec" execF
    addCommand "echo" echoF
    addCommand "cmdlist" listF
    addCommand "alias" aliasF
    addCommand "wait" waitF

addCommand :: B.ByteString -> Quake () -> Quake ()
addCommand cmdName f = undefined -- TODO

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
