{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Game.Cmd where

import Data.Foldable (find)
import Data.Traversable (traverse)
import Data.Sequence ((<|))
import Control.Lens ((^.), (%=))
import Control.Monad.State (liftM, get)
import qualified Data.Sequence as Seq
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

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
    cmdExists <- commandExists cmdName

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
       | otherwise ->
           cmdGlobals.cgCmdFunctions %= (CmdFunctionT cmdName f <|)

  where commandExists name = do
          allCommands <- liftM (^.cmdGlobals.cgCmdFunctions) get
          case find (\cmd -> cmd^.cfName == name) allCommands of
            Just _ -> return True
            Nothing -> return False

execF :: Quake ()
execF = undefined -- TODO

echoF :: Quake ()
echoF = undefined -- TODO

listF :: Quake ()
listF = do
    allCommands <- liftM (^.cmdGlobals.cgCmdFunctions) get
    _ <- traverse (\cf -> Com.printf $ (cf^.cfName) `B.append` "\n") allCommands
    Com.printf $ BC.pack (show $ Seq.length allCommands) `B.append` " commands \n" -- TODO: maybe use Data.Binary for Int to Bytestring conversion ?

aliasF :: Quake ()
aliasF = undefined -- TODO

waitF :: Quake ()
waitF = undefined -- TODO
