module Game.Cmd
  (addCommand
  ,executeString
  ,initialize)
  where

import qualified QCommon.CBufShared as CBuf
import qualified QCommon.Com as Com
import qualified QCommon.CVarShared as CVar
import qualified QCommon.FSShared as FS
import           QuakeState
import           Types

import           Control.Lens (use, (^.), (%=), (.=))
import qualified Data.ByteString as B
import           Data.Maybe (fromMaybe)
import           Data.Serialize (encode)
import           Data.Sequence ((<|))
import qualified Data.Sequence as Seq
import qualified Data.Vector as V

initialCommands :: [(B.ByteString,XCommandT)]
initialCommands =
  [("exec",execF),("echo",echoF),("cmdlist",listF),("alias",aliasF),("wait",waitF)] 
  
initialize :: Quake ()
initialize = mapM_ (\(name,cmd) -> addCommand name (Just cmd)) initialCommands

addCommand :: B.ByteString -> Maybe XCommandT -> Quake ()
addCommand name cmd =
  do variable <- CVar.variableString name
     exists <- commandExists name
     if | not (B.null variable) -> -- fail if the command is a variable name
            Com.printf $ "Cmd.addCommand: " `B.append` name `B.append` " already defined as a var\n"
        | exists -> -- fail if the command already exists
            Com.printf $ "Cmd.addCommand: " `B.append` name `B.append` " already defined\n"
        | otherwise -> -- add new command otherwise
            cmdGlobals.cgCmdFunctions %= (CmdFunctionT name cmd <|)

commandExists :: B.ByteString -> Quake Bool
commandExists name =
  do allCommands <- use $ cmdGlobals.cgCmdFunctions
     return (any (\cmd -> cmd^.cfName == name) allCommands)

execF :: XCommandT
execF = XCommandT "Cmd.execF"
  (do c <- argc
      if c /= 2
        then Com.printf "exec <filename> : execute a script file\n"
        else loadAndExec)
  where loadAndExec =
          do fileName <- argv 1
             script <- FS.loadFile fileName
             maybe (failedToExec fileName) (execScript fileName) script
        failedToExec fileName =
             Com.printf ("couldn't exec " `B.append` fileName `B.append` "\n")
        execScript fileName contents =
          do Com.printf ("execing " `B.append` fileName `B.append` "\n")
             CBuf.insertText contents

echoF :: XCommandT
echoF = XCommandT "Cmd.echoF"
  (do v <- use (cmdGlobals.cgCmdArgv)
      mapM_ (\arg -> Com.printf (arg `B.append` " ")) (V.drop 1 v)
      Com.printf "'\n")

listF :: XCommandT
listF = XCommandT "Cmd.listF"
  (do allCommands <- use (cmdGlobals.cgCmdFunctions)
      mapM_ (\cmd -> Com.printf ((cmd^.cfName) `B.append` "\n")) allCommands
      Com.printf (encode (Seq.length allCommands) `B.append` " commands \n"))
  

aliasF :: XCommandT
aliasF = error "Cmd.aliasF" -- TODO

waitF :: XCommandT
waitF = XCommandT "Cmd.waitF" (globals.gCmdWait .= True)

argc :: Quake Int
argc = use (cmdGlobals.cgCmdArgc)

argv :: Int -> Quake B.ByteString
argv idx =
  do cmdArgv <- use (cmdGlobals.cgCmdArgv)
     return (fromMaybe "" (cmdArgv V.!? idx))

executeString :: B.ByteString -> Quake ()
executeString = error "Cmd.executeString" -- TODO