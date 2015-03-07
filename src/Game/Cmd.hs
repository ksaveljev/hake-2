{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Game.Cmd where

import Data.Foldable (find)
import Data.Traversable (traverse)
import Data.Sequence ((<|))
import Control.Lens ((^.), (%=), (.=), use)
import Control.Monad.State (liftM)
import qualified Data.Sequence as Seq
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V

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
          allCommands <- use $ cmdGlobals.cgCmdFunctions
          case find (\cmd -> cmd^.cfName == name) allCommands of
            Just _ -> return True
            Nothing -> return False

execF :: Quake ()
execF = undefined -- TODO

echoF :: Quake ()
echoF = do
    v <- liftM (V.drop 1) (use $ cmdGlobals.cgCmdArgv)
    _ <- traverse (\arg -> Com.printf $ arg `B.append` " ") v
    Com.printf "'\n"

listF :: Quake ()
listF = do
    allCommands <- use $ cmdGlobals.cgCmdFunctions
    _ <- traverse (\cf -> Com.printf $ (cf^.cfName) `B.append` "\n") allCommands
    Com.printf $ BC.pack (show $ Seq.length allCommands) `B.append` " commands \n" -- TODO: maybe use Data.Binary for Int to Bytestring conversion ?

aliasF :: Quake ()
aliasF = undefined -- TODO

waitF :: Quake ()
waitF = globals.cmdWait .= True

argc :: Quake Int
argc = use $ cmdGlobals.cgCmdArgc

argv :: Int -> Quake B.ByteString
argv idx = do
    c <- argc

    if (idx < 0) || (idx >= c)
      then return ""
      else do
        v <- use $ cmdGlobals.cgCmdArgv
        return $ v V.! idx
