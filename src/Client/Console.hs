module Client.Console
  ( initialize
  ) where

import           Client.ConsoleT
import           Client.VidDefT
import qualified Constants
import qualified Game.Cmd as Cmd
import qualified QCommon.CVar as CVar
import qualified QCommon.Com as Com
import           QuakeState
import           QuakeIOState
import           Types

import           Control.Lens (use, (.=), (^.))
import           Control.Monad (void)
import           Data.Bits (shiftR)
import qualified Data.ByteString as B
import qualified Data.Vector.Storable.Mutable as MSV
import qualified Data.Vector.Unboxed as UV

initialCommands :: [(B.ByteString, Maybe XCommandT)]
initialCommands =
  [ ("toggleconsole", Just toggleConsoleF), ("togglechat", Just toggleChatF)
  , ("messagemode", Just messageModeF), ("messagemode2", Just messageMode2F)
  , ("clear", Just clearF), ("condump", Just dumpF) ]

initialize :: Quake ()
initialize =
  do globals.gCon.cLineWidth .= -1
     checkResize =<< use (globals.gVidDef)
     Com.printf "Console initialized.\n"
     void (CVar.get "con_notifytime" "3" 0)
     Cmd.addInitialCommands initialCommands
     globals.gCon.cInitialized .= True

checkResize :: VidDefT -> Quake ()
checkResize vidDef =
  do lineWidth <- use (globals.gCon.cLineWidth)
     proceedCheckResize width lineWidth
  where w = ((vidDef^.vdWidth) `shiftR` 3) - 2
        width = min w Constants.maxCmdLine

proceedCheckResize :: Int -> Int -> Quake ()
proceedCheckResize width lineWidth
  | width == lineWidth = return ()
  | width < 1 = -- video hasn't been initialized yet
      do globals.gCon.cLineWidth .= 38
         globals.gCon.cTotalLines .= Constants.conTextSize `div` 38
         emptyConsole
         updateConsoleValues
  | otherwise = do
      oldWidth <- use (globals.gCon.cLineWidth)
      oldTotalLines <- use (globals.gCon.cTotalLines)
      doResize width oldWidth oldTotalLines lineWidth
      updateConsoleValues
  where emptyConsole =
          request (do consoleBuffer <- use ioText
                      io (consoleBuffer `MSV.set` ' '))
        updateConsoleValues =
          do totalLines <- use (globals.gCon.cTotalLines)
             globals.gCon.cCurrent .= totalLines - 1
             globals.gCon.cDisplay .= totalLines - 1

doResize :: Int -> Int -> Int -> Int -> Quake ()
doResize width oldWidth oldTotalLines lineWidth =
  do globals.gCon.cLineWidth .= width
     globals.gCon.cTotalLines .= totalLines
     currentLine <- use (globals.gCon.cCurrent)
     request (refillConsoleBuffer width oldWidth totalLines oldTotalLines currentLine lineWidth)
     clearNotify
  where totalLines = Constants.conTextSize `div` width

refillConsoleBuffer :: Int -> Int -> Int -> Int -> Int -> Int -> QuakeIO ()
refillConsoleBuffer width oldWidth totalLines oldTotalLines currentLine lineWidth =
  do consoleBuffer <- use ioText
     io (do tbuf <- MSV.clone consoleBuffer
            fillInBuf consoleBuffer tbuf width oldWidth totalLines oldTotalLines currentLine lineWidth 0 0)

fillInBuf :: MSV.IOVector Char -> MSV.IOVector Char -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()
fillInBuf consoleBuffer tbuf width oldWidth totalLines oldTotalLines currentLine lineWidth i j
  | i >= numLines = return ()
  | j >= numChars = fillInBuf consoleBuffer tbuf width oldWidth totalLines oldTotalLines currentLine lineWidth (i + 1) 0
  | otherwise =
      do let idx = (totalLines - 1 - i) * lineWidth + j
             idx2 = ((currentLine - i + oldTotalLines) `mod` oldTotalLines) * oldWidth + j
         tbuf `MSV.read` idx2 >>= MSV.write consoleBuffer idx
         fillInBuf consoleBuffer tbuf width oldWidth totalLines oldTotalLines currentLine lineWidth i (j + 1)
  where numLines = min totalLines oldTotalLines
        numChars = min width oldWidth

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
  
clearNotify :: Quake ()
clearNotify = globals.gCon.cTimes .= UV.replicate Constants.numConTimes 0