{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Game.Cmd where

import Data.Foldable (find)
import Control.Lens ((^.))
import Control.Monad.State (liftM, get)
import qualified Data.ByteString as B

import Quake
import QuakeState
import QCommon.CmdFunctionT
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
    cmdExists <- commandExists cmdName -- TODO

    -- fail if the command is a variable name
    -- fail if the command already exists
    -- add new command otherwise
    if | B.length varStr > 0 ->
           Com.printf $ "Cmd.addCommand: "
             `B.append` cmdName
             `B.append` " already defined as a var\n"
       | cmdExists ->
           Com.printf $ "Cmd.addCommand: "
             `B.append` cmdName
             `B.append` " already defined\n"
       | otherwise -> undefined -- TODO

  where commandExists name = do
          allFunctions <- liftM (^.cmdGlobals.cgCmdFunctions) get
          case find (\function -> function^.cfName == name) allFunctions of
            Just _ -> return True
            Nothing -> return False

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
