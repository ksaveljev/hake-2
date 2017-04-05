{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
module Client.Console where

import Control.Lens ((.=), use, zoom, (^.), ix, preuse, (-=), (+=))
import Control.Monad (void, unless, when, liftM)
import Data.Bits (shiftR, shiftL, (.&.), (.|.), xor)
import Data.Char (ord, chr)
import Data.Maybe (isJust)
import Data.Word (Word8)
import Text.Printf (printf)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Storable.Mutable as MSV

import Game.CVarT
import Client.ClientStateT
import Client.ClientStaticT
import Client.ConsoleT
import Client.RefExportT
import Render.Renderer
import Types
import QuakeState
import CVarVariables
import QCommon.XCommandT
import qualified Constants
import {-# SOURCE #-} qualified Client.SCR as SCR
import {-# SOURCE #-} qualified Game.Cmd as Cmd
import {-# SOURCE #-} qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar

init :: Quake ()
init = do
    globals.gCon.cLineWidth .= -1

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

    globals.gCon.cInitialized .= True

-- If the line width has changed, reformat the buffer.
checkResize :: Quake ()
checkResize = do
    vidDefWidth <- use $ globals.gVidDef.vdWidth

    let w = (vidDefWidth `shiftR` 3) - 2
        width = if w > Constants.maxCmdLine
                  then Constants.maxCmdLine
                  else w

    lineWidth <- use $ globals.gCon.cLineWidth

    unless (width == lineWidth) $ do
      if width < 1 -- video hasn't been initialized yet
        then do
          zoom (globals.gCon) $ do
            cLineWidth .= 38
            cTotalLines .= Constants.conTextSize `div` 38

          use (globals.gCon.cText) >>= \text ->
            io $ text `MSV.set` ' '

        else do
          oldWidth <- use $ globals.gCon.cLineWidth
          globals.gCon.cLineWidth .= width

          let totalLines = Constants.conTextSize `div` width
          oldTotalLines <- use $ globals.gCon.cTotalLines
          globals.gCon.cTotalLines .= totalLines

          let numLines = if totalLines < oldTotalLines
                           then totalLines
                           else oldTotalLines

          let numChars = if width < oldWidth
                           then width
                           else oldWidth

          currentLine <- use $ globals.gCon.cCurrent
          use (globals.gCon.cText) >>= \text -> do
            tbuf <- io $ MSV.clone text
            fillInBuf text tbuf oldTotalLines oldWidth currentLine totalLines width 0 0 numLines numChars

          clearNotify

      totalLines <- use $ globals.gCon.cTotalLines
      globals.gCon.cCurrent .= totalLines - 1
      globals.gCon.cDisplay .= totalLines - 1

  where fillInBuf :: MSV.IOVector Char -> MSV.IOVector Char -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Quake ()
        fillInBuf text tbuf oldTotalLines oldWidth currentLine totalLines lineWidth i j maxI maxJ
          | i >= maxI = return ()
          | j >= maxJ = fillInBuf text tbuf oldTotalLines oldWidth currentLine totalLines lineWidth (i + 1) 0 maxI maxJ
          | otherwise = do
              let idx = (totalLines - 1 - i) * lineWidth + j
                  idx2 = ((currentLine - i + oldTotalLines) `mod` oldTotalLines) * oldWidth + j
              io $ tbuf `MSV.read` idx2 >>= MSV.write text idx
              fillInBuf text tbuf oldTotalLines oldWidth currentLine totalLines lineWidth i (j + 1) maxI maxJ

toggleConsoleF :: XCommandT
toggleConsoleF =
  XCommandT "Console.toggleConsoleF" (do
    io (putStrLn "Console.toggleConsoleF") >> undefined -- TODO
  )

toggleChatF :: XCommandT
toggleChatF =
  XCommandT "Console.toggleChatF" (do
    io (putStrLn "Console.toggleChatF") >> undefined -- TODO
  )

messageModeF :: XCommandT
messageModeF =
  XCommandT "Console.messageModeF" (do
    globals.gChatTeam .= False
    globals.gCls.csKeyDest .= Constants.keyMessage
  )

messageMode2F :: XCommandT
messageMode2F =
  XCommandT "Console.messageMode2F" (do
    globals.gChatTeam .= True
    globals.gCls.csKeyDest .= Constants.keyMessage
  )

clearF :: XCommandT
clearF =
  XCommandT "Console.clearF" (do
    text <- io $ MSV.replicate Constants.conTextSize ' '
    globals.gCon.cText .= text
  )

dumpF :: XCommandT
dumpF =
  XCommandT "Console.dumpF" (do
    io (putStrLn "Console.dumpF") >> undefined -- TODO
  )

clearNotify :: Quake ()
clearNotify = globals.gCon.cTimes .= UV.replicate Constants.numConTimes 0

drawAltString :: Int -> Int -> B.ByteString -> Quake ()
drawAltString x y s = do
    draw x 0 (B.length s)

  where draw :: Int -> Int -> Int -> Quake ()
        draw x' idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              Just renderer <- use $ globals.gRenderer
              (renderer^.rRefExport.reDrawChar) x' y ((ord $ s `BC.index` idx) `xor` 0x80)
              draw (x' + 8) (idx + 1) maxIdx

drawString :: Int -> Int -> B.ByteString -> Quake ()
drawString x y s =
    draw x 0 (B.length s)

  where draw :: Int -> Int -> Int -> Quake ()
        draw x' idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              Just renderer <- use $ globals.gRenderer
              (renderer^.rRefExport.reDrawChar) x' y (ord $ s `BC.index` idx)
              draw (x' + 8) (idx + 1) maxIdx

{-
- ================ Con_DrawConsole
- 
- Draws the console with the solid background ================
-}
drawConsole :: Float -> Quake ()
drawConsole frac = do
    vidDef' <- use $ globals.gVidDef

    let width = vidDef'^.vdWidth
        height = vidDef'^.vdHeight
        tmpLinesNum = truncate ((fromIntegral height) * frac)

    unless (tmpLinesNum <= 0) $ do
      let linesNum = if tmpLinesNum > height
                       then height
                       else tmpLinesNum

      -- draw the background
      Just renderer <- use $ globals.gRenderer
      (renderer^.rRefExport.reDrawStretchPic) 0 (-height + linesNum) width height "conback"
      SCR.addDirtyPoint 0 0
      SCR.addDirtyPoint (width - 1) (linesNum - 1)

      let version = BC.pack (printf "v%4.2f" Constants.version)
      drawVersion version width linesNum 0 5

      -- draw the text
      globals.gCon.cVisLines .= linesNum
      console <- use $ globals.gCon

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

      download <- use $ globals.gCls.csDownload

      when (isJust download) $ do
        io (putStrLn "Console.drawConsole") >> undefined -- TODO

      -- draw the input prompt, user text, and cursor if desired
      drawInput

  where drawVersion :: B.ByteString -> Int -> Int -> Int -> Int -> Quake ()
        drawVersion version width linesNum idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              Just renderer <- use $ globals.gRenderer
              (renderer^.rRefExport.reDrawChar) (width - 44 + idx * 8) (linesNum - 12) (128 + ord (BC.index version idx))
              drawVersion version width linesNum (idx + 1) maxIdx

        drawArrows :: Int -> Int -> Int -> Quake ()
        drawArrows x maxX y
          | x >= maxX = return ()
          | otherwise = do
              Just renderer <- use $ globals.gRenderer
              (renderer^.rRefExport.reDrawChar) ((x + 1) `shiftL` 3) y (ord '^')
              drawArrows (x + 4) maxX y

        drawText :: ConsoleT -> Int -> Int -> Int -> Int -> Quake ()
        drawText console row y idx maxIdx
          | idx >= maxIdx || row < 0 || (console^.cCurrent) - row >= (console^.cTotalLines) = return ()
          | otherwise = do
              Just renderer <- use $ globals.gRenderer
              let first = (row `mod` (console^.cTotalLines)) * (console^.cLineWidth)
                  drawChar = renderer^.rRefExport.reDrawChar
              drawLine drawChar (console^.cText) first y 0 (console^.cLineWidth)
              drawText console (row - 1) (y - 8) (idx + 1) maxIdx

        drawLine :: (Int -> Int -> Int -> Quake ()) -> MSV.IOVector Char -> Int -> Int -> Int -> Int -> Quake ()
        drawLine drawChar text first y idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              ch <- io $ text `MSV.read` (idx + first)
              drawChar ((idx + 1) `shiftL` 3) y $! (ord ch)
              drawLine drawChar text first y (idx + 1) maxIdx

{-
- ================ Con_DrawNotify ================
- 
- Draws the last few lines of output transparently over the game top
-}
drawNotify :: Quake ()
drawNotify = do
    console <- use $ globals.gCon
    vidDefWidth <- use $ globals.gVidDef.vdWidth
    cls' <- use $ globals.gCls

    v <- draw cls' console 0 ((console^.cCurrent) - Constants.numConTimes + 1) (console^.cCurrent)

    v' <- if (cls'^.csKeyDest) == Constants.keyMessage
            then do
              chatTeam' <- use $ globals.gChatTeam
              skip <- if chatTeam'
                        then do
                          drawString 8 v "say_team:"
                          return 11
                        else do
                          drawString 8 v "say:"
                          return 5

              s <- use $ globals.gChatBuffer
              chatLen <- use $ globals.gChatBufferLen
              let s' = if chatLen > (vidDefWidth `shiftR` 3) - (skip + 1)
                         then B.drop (chatLen - ((vidDefWidth `shiftR` 3) - (skip + 1))) s 
                         else s
              drawChat s' skip v 0 (B.length s')
              return (v + 8)
            else
              return v

    when (v' /= 0) $ do
      SCR.addDirtyPoint 0 0
      SCR.addDirtyPoint (vidDefWidth - 1) v'

  where draw :: ClientStaticT -> ConsoleT -> Int -> Int -> Int -> Quake Int
        draw cls' console v idx maxIdx
          | idx > maxIdx = return v
          | idx < 0 = draw cls' console v (idx + 1) maxIdx
          | otherwise = do
              let time = truncate ((console^.cTimes) UV.! (idx `mod` Constants.numConTimes)) :: Int

              if time == 0
                then draw cls' console v (idx + 1) maxIdx
                else do
                  let time' = (cls'^.csRealTime) - time
                  conNotifyTimeValue <- liftM (^.cvValue) conNotifyTimeCVar
                  if time' > truncate (conNotifyTimeValue * 1000)
                    then draw cls' console v (idx + 1) maxIdx
                    else do
                      let text = (idx `mod` (console^.cTotalLines)) * (console^.cLineWidth)
                      drawText console v text 0 (console^.cLineWidth)
                      draw cls' console (v + 8) (idx + 1) maxIdx

        drawText :: ConsoleT -> Int -> Int -> Int -> Int -> Quake ()
        drawText console v text idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              Just renderer <- use $ globals.gRenderer
              c <- io $ MSV.read (console^.cText) (text + idx)
              (renderer^.rRefExport.reDrawChar) ((idx + 1) `shiftL` 3) v (ord c)
              drawText console v text (idx + 1) maxIdx

        drawChat :: B.ByteString -> Int -> Int -> Int -> Int -> Quake ()
        drawChat s skip v idx maxIdx
          | idx >= maxIdx = do
              realTime <- use $ globals.gCls.csRealTime
              Just renderer <- use $ globals.gRenderer
              (renderer^.rRefExport.reDrawChar) ((idx + skip) `shiftL` 3) v (10 + ((realTime `shiftR` 8) .&. 1))
          | otherwise = do
              Just renderer <- use $ globals.gRenderer
              (renderer^.rRefExport.reDrawChar) ((idx + skip) `shiftL` 3) v (ord $ s `BC.index` idx)
              drawChat s skip v (idx + 1) maxIdx
              

{-
- ================ Con_DrawInput
- 
- The input line scrolls horizontally if typing goes beyond the right edge
- ================
-}
drawInput :: Quake ()
drawInput = do
    keyDest <- use $ globals.gCls.csKeyDest
    state <- use $ globals.gCls.csState
                                            -- don't draw anything (always draw if not active)
    unless (keyDest == Constants.keyMenu || (keyDest /= Constants.keyConsole && state == Constants.caActive)) $ do
      editLine' <- use $ globals.gEditLine
      Just text <- preuse $ globals.gKeyLines.ix editLine'
      linePos <- use $ globals.gKeyLinePos
      lineWidth <- use $ globals.gCon.cLineWidth

      -- add the cursor frame and fill out remainder with spaces
      realTime <- use $ globals.gCls.csRealTime
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
      visLines <- use $ globals.gCon.cVisLines
      Just renderer <- use $ globals.gRenderer
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
    initialized <- use $ globals.gCon.cInitialized

    when initialized $ do
      let (mask, txtpos) = if txt `B.index` 0 == 1 || txt `B.index` 0 == 2
                             then (128, 1)
                             else (0, 0)

      printTxt txtpos mask

  where printTxt :: Int -> Int -> Quake ()
        printTxt txtpos mask
          | txtpos >= B.length txt = return ()
          | otherwise = do
              console <- use $ globals.gCon

              let c = txt `BC.index` txtpos
                  len = findWordLen (console^.cLineWidth) txtpos 0

              -- word wrap
              when (len /= (console^.cLineWidth) && (console^.cX) + len > (console^.cLineWidth)) $
                globals.gCon.cX .= 0

              use (clientGlobals.cgCR) >>= \v ->
                when (v /= 0) $ do
                  globals.gCon.cCurrent -= 1
                  clientGlobals.cgCR .= 0

              use (globals.gCon) >>= \console' ->
                when ((console'^.cX) == 0) $ do
                  lineFeed
                  -- mark time for transparent overlay
                  when ((console'^.cCurrent) >= 0) $ do
                    realTime <- use $ globals.gCls.csRealTime
                    globals.gCon.cTimes.ix ((console'^.cCurrent) `mod` Constants.numConTimes) .= fromIntegral realTime

              case c of
                '\n' -> do
                  globals.gCon.cX .= 0
                  printTxt (txtpos + 1) mask

                '\r' -> do
                  globals.gCon.cX .= 0
                  clientGlobals.cgCR .= 1
                  printTxt (txtpos + 1) mask

                _ -> -- display character and advance
                  use (globals.gCon) >>= \console' -> do
                    let y = (console'^.cCurrent) `mod` (console'^.cTotalLines)
                        idx = y * (console'^.cLineWidth) + (console'^.cX)
                        b = (ord c) .|. mask .|. (console'^.cOrMask)
                    io $ MSV.write (console'^.cText) idx (chr b)
                    globals.gCon.cX += 1
                    when ((console'^.cX) + 1 >= (console'^.cLineWidth)) $
                      globals.gCon.cX .= 0
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
    globals.gCon.cX .= 0

    use (globals.gCon) >>= \console ->
      when ((console^.cDisplay) == (console^.cCurrent)) $
        globals.gCon.cDisplay += 1

    globals.gCon.cCurrent += 1

    use (globals.gCon) >>= \console -> do
      let i = ((console^.cCurrent) `mod` (console^.cTotalLines)) * (console^.cLineWidth)
          e = i + (console^.cLineWidth)
      fillSpaces (console^.cText) i e

  where fillSpaces :: MSV.IOVector Char -> Int -> Int -> Quake ()
        fillSpaces text i e
          | i >= e = return ()
          | otherwise = do
              io $ MSV.write text i ' '
              fillSpaces text (i + 1) e
