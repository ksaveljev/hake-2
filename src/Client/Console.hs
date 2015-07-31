{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
module Client.Console where

import Control.Lens ((.=), use, zoom, (^.), ix, preuse, (-=), (+=))
import Control.Monad (void, unless, when)
import Data.Bits (shiftR, shiftL, (.&.), (.|.), xor)
import Data.Char (ord, chr)
import Data.Maybe (isJust)
import Data.Word (Word8)
import Text.Printf (printf)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Storable.Mutable as MV

import Quake
import QuakeState
import QCommon.XCommandT
import qualified Constants
import {-# SOURCE #-} qualified Client.SCR as SCR
import {-# SOURCE #-} qualified Game.Cmd as Cmd
import qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar

init :: Quake ()
init = do
    globals.con.cLineWidth .= -1

    checkResize

    Com.printf "Console initialized.\n"

    -- register our commands
    void $ CVar.get "con_notifytime" "3" 0

    Cmd.addCommand "toggleconsole" (Just toggleConsoleF)
    Cmd.addCommand "togglechat" (Just toggleChatF)
    Cmd.addCommand "messagemode" (Just messageModeF)
    Cmd.addCommand "messagemode2" (Just messageMode2F)
    Cmd.addCommand "clear" (Just clearF)
    Cmd.addCommand "condump" (Just dumpF)

    globals.con.cInitialized .= True

-- If the line width has changed, reformat the buffer.
checkResize :: Quake ()
checkResize = do
    vidDefWidth <- use $ globals.vidDef.vdWidth

    let w = (vidDefWidth `shiftR` 3) - 2
        width = if w > Constants.maxCmdLine
                  then Constants.maxCmdLine
                  else w

    lineWidth <- use $ globals.con.cLineWidth

    unless (width == lineWidth) $ do
      if width < 1 -- video hasn't been initialized yet
        then do
          zoom (globals.con) $ do
            cLineWidth .= 38
            cTotalLines .= Constants.conTextSize `div` 38

          use (globals.con.cText) >>= \text ->
            io $ text `MV.set` ' '

        else do
          oldWidth <- use $ globals.con.cLineWidth
          globals.con.cLineWidth .= width

          let totalLines = Constants.conTextSize `div` width
          oldTotalLines <- use $ globals.con.cTotalLines
          globals.con.cTotalLines .= totalLines

          let numLines = if totalLines < oldTotalLines
                           then totalLines
                           else oldTotalLines

          let numChars = if width < oldWidth
                           then width
                           else oldWidth

          currentLine <- use $ globals.con.cCurrent
          use (globals.con.cText) >>= \text -> do
            tbuf <- io $ MV.clone text
            fillInBuf text tbuf oldTotalLines oldWidth currentLine totalLines width 0 0 numLines numChars

          clearNotify

      totalLines <- use $ globals.con.cTotalLines
      globals.con.cCurrent .= totalLines - 1
      globals.con.cDisplay .= totalLines - 1

  where fillInBuf :: MV.IOVector Char -> MV.IOVector Char -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Quake ()
        fillInBuf text tbuf oldTotalLines oldWidth currentLine totalLines lineWidth i j maxI maxJ
          | i >= maxI = return ()
          | j >= maxJ = fillInBuf text tbuf oldTotalLines oldWidth currentLine totalLines lineWidth (i + 1) 0 maxI maxJ
          | otherwise = do
              let idx = (totalLines - 1 - i) * lineWidth + j
                  idx2 = ((currentLine - i + oldTotalLines) `mod` oldTotalLines) * oldWidth + j
              io $ tbuf `MV.read` idx2 >>= MV.write text idx
              fillInBuf text tbuf oldTotalLines oldWidth currentLine totalLines lineWidth i (j + 1) maxI maxJ

toggleConsoleF :: XCommandT
toggleConsoleF = io (putStrLn "Console.toggleConsoleF") >> undefined -- TODO

toggleChatF :: XCommandT
toggleChatF = io (putStrLn "Console.toggleChatF") >> undefined -- TODO

messageModeF :: XCommandT
messageModeF = do
    globals.chatTeam .= False
    globals.cls.csKeyDest .= Constants.keyMessage

messageMode2F :: XCommandT
messageMode2F = do
    globals.chatTeam .= True
    globals.cls.csKeyDest .= Constants.keyMessage

clearF :: XCommandT
clearF = do
    text <- io $ MV.replicate Constants.conTextSize ' '
    globals.con.cText .= text

dumpF :: XCommandT
dumpF = io (putStrLn "Console.dumpF") >> undefined -- TODO

clearNotify :: Quake ()
clearNotify = globals.con.cTimes .= UV.replicate Constants.numConTimes 0

drawAltString :: Int -> Int -> B.ByteString -> Quake ()
drawAltString x y s = do
    draw x 0 (B.length s)

  where draw :: Int -> Int -> Int -> Quake ()
        draw x' idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              Just renderer <- use $ globals.re
              (renderer^.rRefExport.reDrawChar) x' y ((ord $ s `BC.index` idx) `xor` 0x80)
              draw (x' + 8) (idx + 1) maxIdx

drawString :: Int -> Int -> B.ByteString -> Quake ()
drawString x y s =
    draw x 0 (B.length s)

  where draw :: Int -> Int -> Int -> Quake ()
        draw x' idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              Just renderer <- use $ globals.re
              (renderer^.rRefExport.reDrawChar) x' y (ord $ s `BC.index` idx)
              draw (x' + 8) (idx + 1) maxIdx

{-
- ================ Con_DrawConsole
- 
- Draws the console with the solid background ================
-}
drawConsole :: Float -> Quake ()
drawConsole frac = do
    vidDef' <- use $ globals.vidDef

    let width = vidDef'^.vdWidth
        height = vidDef'^.vdHeight
        tmpLinesNum = truncate ((fromIntegral height) * frac)

    unless (tmpLinesNum <= 0) $ do
      let linesNum = if tmpLinesNum > height
                       then height
                       else tmpLinesNum

      -- draw the background
      Just renderer <- use $ globals.re
      (renderer^.rRefExport.reDrawStretchPic) 0 (-height + linesNum) width height "conback"
      SCR.addDirtyPoint 0 0
      SCR.addDirtyPoint (width - 1) (linesNum - 1)

      let version = BC.pack (printf "v%4.2f" Constants.version)
      drawVersion version width linesNum 0 5

      -- draw the text
      globals.con.cVisLines .= linesNum
      console <- use $ globals.con

      (rows, y) <- do
        let rows = (linesNum - 22) `shiftR` 3 -- rows of text to draw
            y = linesNum - 30

        -- draw from the bottom up
        if (console^.cDisplay) /= (console^.cCurrent)
          then do
            -- draw arrows to show the buffer is backscrolled
            drawArrows 0 (console^.cLineWidth) y
            return (rows - 1, y - 8)
          else
            return (rows, y)

      let row = console^.cDisplay
      drawText console row y 0 rows

      download <- use $ globals.cls.csDownload

      when (isJust download) $ do
        io (putStrLn "Console.drawConsole") >> undefined -- TODO

      -- draw the input prompt, user text, and cursor if desired
      drawInput

  where drawVersion :: B.ByteString -> Int -> Int -> Int -> Int -> Quake ()
        drawVersion version width linesNum idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              Just renderer <- use $ globals.re
              (renderer^.rRefExport.reDrawChar) (width - 44 + idx * 8) (linesNum - 12) (128 + ord (BC.index version idx))
              drawVersion version width linesNum (idx + 1) maxIdx

        drawArrows :: Int -> Int -> Int -> Quake ()
        drawArrows x maxX y
          | x >= maxX = return ()
          | otherwise = do
              Just renderer <- use $ globals.re
              (renderer^.rRefExport.reDrawChar) ((x + 1) `shiftL` 3) y (ord '^')
              drawArrows (x + 4) maxX y

        drawText :: ConsoleT -> Int -> Int -> Int -> Int -> Quake ()
        drawText console row y idx maxIdx
          | idx >= maxIdx || row < 0 || (console^.cCurrent) - row >= (console^.cTotalLines) = return ()
          | otherwise = do
              Just renderer <- use $ globals.re
              let first = (row `mod` (console^.cTotalLines)) * (console^.cLineWidth)
                  drawChar = renderer^.rRefExport.reDrawChar
              drawLine drawChar (console^.cText) first y 0 (console^.cLineWidth)
              drawText console (row - 1) (y - 8) (idx + 1) maxIdx

        drawLine :: (Int -> Int -> Int -> Quake ()) -> MV.IOVector Char -> Int -> Int -> Int -> Int -> Quake ()
        drawLine drawChar text first y idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              ch <- io $ text `MV.read` (idx + first)
              drawChar ((idx + 1) `shiftL` 3) y $! (ord ch)
              drawLine drawChar text first y (idx + 1) maxIdx

{-
- ================ Con_DrawNotify ================
- 
- Draws the last few lines of output transparently over the game top
-}
drawNotify :: Quake ()
drawNotify = do
    io (putStrLn "Console.drawNotify") >> undefined -- TODO

{-
- ================ Con_DrawInput
- 
- The input line scrolls horizontally if typing goes beyond the right edge
- ================
-}
drawInput :: Quake ()
drawInput = do
    keyDest <- use $ globals.cls.csKeyDest
    state <- use $ globals.cls.csState
                                            -- don't draw anything (always draw if not active)
    unless (keyDest == Constants.keyMenu || (keyDest /= Constants.keyConsole && state == Constants.caActive)) $ do
      editLine' <- use $ globals.editLine
      Just text <- preuse $ globals.keyLines.ix editLine'
      linePos <- use $ globals.keyLinePos
      lineWidth <- use $ globals.con.cLineWidth

      -- add the cursor frame and fill out remainder with spaces
      realTime <- use $ globals.cls.csRealTime
      let cursorFrame :: Word8 = fromIntegral $ 10 + ((realTime `shiftR` 8) .&. 1)
          fullLine = text `B.append` B.unfoldr (\i -> if i == 0 
                                                        then Just (cursorFrame, 1) 
                                                        else if linePos + 1 + i < lineWidth 
                                                               then Just (32, i + 1)
                                                               else Nothing) 0

      -- prestep if horizontally scrolling
      let start = if linePos >= lineWidth
                    then 1 + linePos - lineWidth
                    else 0

      -- draw it
      visLines <- use $ globals.con.cVisLines
      Just renderer <- use $ globals.re
      let drawChar = renderer^.rRefExport.reDrawChar
      drawInputLine drawChar fullLine (visLines - 22) start lineWidth -- TODO: make sure start is here (jake2 bug with not using start?)

  where drawInputLine :: (Int -> Int -> Int -> Quake ()) -> B.ByteString -> Int -> Int -> Int -> Quake ()
        drawInputLine drawChar text y idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              drawChar ((idx + 1) `shiftL` 3) y (ord $ BC.index text idx)
              drawInputLine drawChar text y (idx + 1) maxIdx

{-
- ================ Con_Print
- 
- Handles cursor positioning, line wrapping, etc All console printing must
- go through this in order to be logged to disk If no console is visible,
- the text will appear at the top of the game window ================
-}
print :: B.ByteString -> Quake ()
print txt = do
    initialized <- use $ globals.con.cInitialized

    when initialized $ do
      let (mask, txtpos) = if txt `B.index` 0 == 1 || txt `B.index` 0 == 2
                             then (128, 1)
                             else (0, 0)

      printTxt txtpos mask

  where printTxt :: Int -> Int -> Quake ()
        printTxt txtpos mask
          | txtpos >= B.length txt = return ()
          | otherwise = do
              console <- use $ globals.con

              let c = txt `BC.index` txtpos
                  len = findWordLen (console^.cLineWidth) txtpos 0

              -- word wrap
              when (len /= (console^.cLineWidth) && (console^.cX) + len > (console^.cLineWidth)) $
                globals.con.cX .= 0

              use (clientGlobals.cgCR) >>= \v ->
                when (v /= 0) $ do
                  globals.con.cCurrent -= 1
                  clientGlobals.cgCR .= 0

              use (globals.con) >>= \console' ->
                when ((console'^.cX) == 0) $ do
                  lineFeed
                  -- mark time for transparent overlay
                  when ((console'^.cCurrent) >= 0) $ do
                    realTime <- use $ globals.cls.csRealTime
                    globals.con.cTimes.ix ((console'^.cCurrent) `mod` Constants.numConTimes) .= fromIntegral realTime

              case c of
                '\n' -> do
                  globals.con.cX .= 0
                  printTxt (txtpos + 1) mask

                '\r' -> do
                  globals.con.cX .= 0
                  clientGlobals.cgCR .= 1
                  printTxt (txtpos + 1) mask

                _ -> -- display character and advance
                  use (globals.con) >>= \console' -> do
                    let y = (console'^.cCurrent) `mod` (console'^.cTotalLines)
                        idx = y * (console'^.cLineWidth) + (console'^.cX)
                        b = (ord c) .|. mask .|. (console'^.cOrMask)
                    io $ MV.write (console'^.cText) idx (chr b)
                    globals.con.cX += 1
                    when ((console'^.cX) + 1 >= (console'^.cLineWidth)) $
                      globals.con.cX .= 0
                    printTxt (txtpos + 1) mask

        findWordLen :: Int -> Int -> Int -> Int
        findWordLen lineWidth txtpos idx =
          if idx >= lineWidth || idx >= (B.length txt - txtpos)
            then idx
            else if txt `BC.index` (idx + txtpos) <= ' '
                   then idx
                   else findWordLen lineWidth txtpos (idx + 1)

lineFeed :: Quake ()
lineFeed = do
    globals.con.cX .= 0

    use (globals.con) >>= \console ->
      when ((console^.cDisplay) == (console^.cCurrent)) $
        globals.con.cDisplay += 1

    globals.con.cCurrent += 1

    use (globals.con) >>= \console -> do
      let i = ((console^.cCurrent) `mod` (console^.cTotalLines)) * (console^.cLineWidth)
          e = i + (console^.cLineWidth)
      fillSpaces (console^.cText) i e

  where fillSpaces :: MV.IOVector Char -> Int -> Int -> Quake ()
        fillSpaces text i e
          | i >= e = return ()
          | otherwise = do
              io $ MV.write text i ' '
              fillSpaces text (i + 1) e
