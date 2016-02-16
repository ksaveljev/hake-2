module QCommon.CBuf
  (addEarlyCommands
  ,addLateCommands
  ,addText
  ,execute
  ,initialize)
  where

import qualified Game.Cmd as Cmd
import qualified QCommon.Com as Com
import qualified QCommon.SZ as SZ
import           QuakeState
import           Types

import           Control.Lens (use, (^.), (.=))
import           Control.Monad (when)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V

addEarlyCommands :: Bool -> Quake ()
addEarlyCommands shouldClear =
  do args <- use (comGlobals.cgComArgv)
     addCommands args 0
  where addCommands args idx
          | idx >= V.length args = return ()
          | otherwise =
              if (args V.! idx) == "+set"
                then do addCommand idx
                        when shouldClear (clearArgs idx)
                        addCommands args (idx + 3)
                else addCommands args (idx + 1)
        addCommand idx =
          do name <- Com.argv (idx + 1)
             value <- Com.argv (idx + 2)
             addText ("set " `B.append` name `B.append` " " `B.append` value `B.append` "\n")
        clearArgs idx = mapM_ Com.clearArgv [idx, idx + 1, idx + 2]

addLateCommands :: Quake Bool
addLateCommands =
  do args <- fmap (V.drop 1) (use (comGlobals.cgComArgv))
     if any (not . B.null) args
       then checkArgumentCommands args
       else return False
  where checkArgumentCommands args =
          do let text = B.intercalate " " (V.toList args)
                 commands = findCommands text 0 ""
                 result = B.length commands /= 0
             when result (addText commands)
             return result
        findCommands text idx accum
          | idx >= B.length text = accum
          | text `BC.index` idx == '+' =
              let command = BC.takeWhile (\ch -> ch /= '+' && ch /= '-') (B.drop (idx + 1) text)
              in findCommands text (idx + B.length command) (accum `B.append` command `B.append` "\n")
          | otherwise = findCommands text (idx + 1) accum

addText :: B.ByteString -> Quake ()
addText text =
  do cmdText <- use (globals.gCmdText)
     if (cmdText^.sbCurSize) + len >= (cmdText^.sbMaxSize)
       then Com.printf "Cbuf.addText: overflow\n"
       else SZ.write (globals.gCmdText) text len
  where len = B.length text

execute :: Quake ()
execute =
  do globals.gAliasCount .= 0
     executeCmdText
  where executeCmdText =
          do cmdText <- use (globals.gCmdText)
             when ((cmdText^.sbCurSize) > 0) $
               execCommands (cmdText^.sbData) (cmdText^.sbCurSize) 0 0
        execCommands :: B.ByteString -> Int -> Int -> Int -> Quake () -- avoid warning when using 'even quotes'
        execCommands text curSize idx quotes
          | idx == curSize = finishExecution text
          | BC.index text idx == '"' = execCommands text curSize (idx + 1) (quotes + 1)
          | (BC.index text idx == ';' && even quotes) || BC.index text idx == '\n' = execCommand text curSize idx
          | otherwise = execCommands text curSize (idx + 1) quotes
        finishExecution text =
          do updateSizeBuf (globals.gCmdText) 0 ""
             Cmd.executeString text
             globals.gCmdWait .= False
        execCommand text curSize idx =
          do let line = B.take idx text -- do not include ';' or '\n'
             updateSizeBuf (globals.gCmdText) (curSize - (idx + 1)) (B.drop (idx + 1) text)
             Cmd.executeString line
             wait <- use (globals.gCmdWait)
             if wait then globals.gCmdWait .= False else executeCmdText -- skip out while text still remains in buffer, leaving it for next frame
        updateSizeBuf sbLens curSize text =
          do sbLens.sbCurSize .= curSize
             sbLens.sbData .= text

initialize :: Quake ()
initialize = SZ.initialize (globals.gCmdText) "" 8192