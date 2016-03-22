module Client.Console
  ( checkResize
  , clearNotify
  , drawConsole
  , drawNotify
  , initialize
  , toggleConsoleF
  ) where

import           Client.ClientStaticT
import           Client.ConsoleT
import           Client.RefExportT
import qualified Client.SCRShared as SCR
import           Client.VidDefT
import qualified Constants
import qualified Game.Cmd as Cmd
import qualified QCommon.CVar as CVar
import qualified QCommon.Com as Com
import           QuakeState
import           QuakeIOState
import           Render.Renderer
import           Types
import           Util.Unsafe

import           Control.Lens (use, (.=), (^.))
import           Control.Monad (void, unless)
import           Data.Bits (shiftR, shiftL)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Data.Char (ord)
import qualified Data.Vector.Storable.Mutable as MSV
import qualified Data.Vector.Unboxed as UV
import           System.IO (Handle)
import           Text.Printf (printf)

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
          request (do consoleBuffer <- use cText
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
  do consoleBuffer <- use cText
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

drawConsole :: Float -> VidDefT -> Quake ()
drawConsole frac vidDef =
  do renderer <- use (globals.gRenderer)
     maybe rendererError (doDrawConsole frac vidDef) renderer
  where rendererError = Com.fatalError "Console.drawConsole renderer is Nothing"

doDrawConsole :: Float -> VidDefT -> Renderer -> Quake ()
doDrawConsole frac vidDef renderer =
  unless (tmpLinesNum <= 0) $
    do (renderer^.rRefExport.reDrawStretchPic) 0 (-height + linesNum) width height "conback"
       SCR.addDirtyPoint 0 0
       SCR.addDirtyPoint (width - 1) (linesNum - 1)
       drawVersion renderer version width linesNum 0 5
       globals.gCon.cVisLines .= linesNum
       console <- use (globals.gCon)
       (rows, y) <- calcRowsAndDrawArrows renderer console linesNum
       drawText renderer console (console^.cDisplay) y 0 rows
       checkDownload =<< use (globals.gCls.csDownload)
       drawInput renderer
  where width = vidDef^.vdWidth
        height = vidDef^.vdHeight
        tmpLinesNum = truncate ((fromIntegral height) * frac)
        linesNum | tmpLinesNum > height = height
                 | otherwise = tmpLinesNum
        version = BC.pack (printf "v%4.2f" Constants.version)
        
drawText :: Renderer -> ConsoleT -> Int -> Int -> Int -> Int -> Quake ()
drawText renderer console row y idx maxIdx
  | idx >= maxIdx || row < 0 || (console^.cCurrent) - row >= (console^.cTotalLines) = return ()
  | otherwise = do
      text <- fmap (mVectorToByteString 'a') (request (use cText))
      drawLine drawChar text first y 0 (console^.cLineWidth)
      drawText renderer console (row - 1) (y - 8) (idx + 1) maxIdx
  where first = (row `mod` (console^.cTotalLines)) * (console^.cLineWidth)
        drawChar = renderer^.rRefExport.reDrawChar

drawLine :: (Int -> Int -> Int -> Quake ()) -> B.ByteString -> Int -> Int -> Int -> Int -> Quake ()
drawLine drawChar text first y idx maxIdx
  | idx >= maxIdx = return ()
  | otherwise = do
      let ch = text `BC.index` (idx + first)
      drawChar ((idx + 1) `shiftL` 3) y $! (ord ch)
      drawLine drawChar text first y (idx + 1) maxIdx

-- IMPROVE: mapM_ or something
drawVersion :: Renderer -> B.ByteString -> Int -> Int -> Int -> Int -> Quake ()
drawVersion renderer version width linesNum idx maxIdx
  | idx >= maxIdx = return ()
  | otherwise = do
      (renderer^.rRefExport.reDrawChar) (width - 44 + idx * 8) (linesNum - 12) (128 + ord (BC.index version idx))
      drawVersion renderer version width linesNum (idx + 1) maxIdx

calcRowsAndDrawArrows :: Renderer -> ConsoleT -> Int -> Quake (Int, Int)
calcRowsAndDrawArrows renderer console linesNum
  | (console^.cDisplay) /= (console^.cCurrent) =
      do drawArrows renderer 0 (console^.cLineWidth) y
         return (rows - 1, y - 8)
  | otherwise = return (rows, y)
  where rows = (linesNum - 22) `shiftR` 3
        y = linesNum - 30
        
drawArrows :: Renderer -> Int -> Int -> Int -> Quake ()
drawArrows renderer x maxX y
  | x >= maxX = return ()
  | otherwise = do
      (renderer^.rRefExport.reDrawChar) ((x + 1) `shiftL` 3) y (ord '^')
      drawArrows renderer (x + 4) maxX y

checkDownload :: Maybe Handle -> Quake ()
checkDownload Nothing = return ()
checkDownload _ = error "Console.checkDownload" -- TODO

drawNotify :: Quake ()
drawNotify = error "Console.drawNotify" -- TODO

drawInput :: Renderer -> Quake ()
drawInput = error "Console.drawInput" -- TODO