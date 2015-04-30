{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Client.Console where

import Control.Lens ((.=), use, zoom, (^.))
import Control.Monad (void, unless, when)
import Data.Bits (shiftR, shiftL)
import Data.Char (ord)
import Data.Maybe (isJust)
import Text.Printf (printf)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector.Unboxed as UV

import Quake
import QuakeState
import QCommon.XCommandT
import qualified Constants
import qualified Client.SCR as SCR
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
        then
          zoom (globals.con) $ do
            cLineWidth .= 38
            cTotalLines .= Constants.conTextSize `div` 38
            cText .= BC.replicate Constants.conTextSize ' '
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

          tbuf <- use $ globals.con.cText
          currentLine <- use $ globals.con.cCurrent
          let updatedBuf = fillInBuf oldTotalLines oldWidth currentLine totalLines width tbuf [] 0 0 numLines numChars
          globals.con.cText .= (BC.pack $ UV.toList updatedBuf) -- IMPROVE: performance?

          clearNotify

      totalLines <- use $ globals.con.cTotalLines
      globals.con.cCurrent .= totalLines - 1
      globals.con.cDisplay .= totalLines - 1

        -- IMPROVE: em, can we optimize it? some other approach maybe?
  where fillInBuf :: Int -> Int -> Int -> Int -> Int -> B.ByteString -> [(Int, Char)] -> Int -> Int -> Int -> Int -> UV.Vector Char
        fillInBuf oldTotalLines oldWidth currentLine totalLines lineWidth tbuf buf i j maxI maxJ
          | i >= maxI = (UV.replicate Constants.conTextSize ' ') UV.// buf
          | j >= maxJ = fillInBuf oldTotalLines oldWidth currentLine totalLines lineWidth tbuf buf (i + 1) 0 maxI maxJ
          | otherwise =
              let idx = (totalLines - 1 - i) * lineWidth + j
                  idx2 = ((currentLine - i + oldTotalLines) `mod` oldTotalLines) * oldWidth + j
              in fillInBuf oldTotalLines oldWidth currentLine totalLines lineWidth tbuf ((idx, BC.index tbuf idx2):buf) i (j + 1) maxI maxJ

toggleConsoleF :: XCommandT
toggleConsoleF = io (putStrLn "Console.toggleConsoleF") >> undefined -- TODO

toggleChatF :: XCommandT
toggleChatF = io (putStrLn "Console.toggleChatF") >> undefined -- TODO

messageModeF :: XCommandT
messageModeF = io (putStrLn "Console.messageModeF") >> undefined -- TODO

messageMode2F :: XCommandT
messageMode2F = io (putStrLn "Console.messageMode2F") >> undefined -- TODO

clearF :: XCommandT
clearF = io (putStrLn "Console.clearF") >> undefined -- TODO

dumpF :: XCommandT
dumpF = io (putStrLn "Console.dumpF") >> undefined -- TODO

clearNotify :: Quake ()
clearNotify = globals.con.cTimes .= UV.replicate Constants.numConTimes 0

drawAltString :: Int -> Int -> B.ByteString -> Quake ()
drawAltString _ _ _ = do
    io (putStrLn "Console.drawAltString") >> undefined -- TODO

drawString :: Int -> Int -> B.ByteString -> Quake ()
drawString _ _ _ = do
    io (putStrLn "Console.drawString") >> undefined -- TODO

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
              let first = (row `mod` (console^.cTotalLines)) * (console^.cLineWidth)
              drawLine (console^.cText) first y 0 (console^.cLineWidth)
              drawText console (row - 1) (y - 8) (idx + 1) maxIdx

        drawLine :: B.ByteString -> Int -> Int -> Int -> Int -> Quake ()
        drawLine text first y idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              Just renderer <- use $ globals.re
              (renderer^.rRefExport.reDrawChar) ((idx + 1) `shiftL` 3) y (ord $ BC.index text (idx + first))
              drawLine text first y (idx + 1) maxIdx

drawNotify :: Quake ()
drawNotify = do
    io (putStrLn "Console.drawNotify") >> undefined -- TODO

drawInput :: Quake ()
drawInput = do
    io (putStrLn "Console.drawInput") >> undefined -- TODO
