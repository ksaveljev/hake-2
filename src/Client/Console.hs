{-# LANGUAGE OverloadedStrings #-}
module Client.Console where

import Control.Lens ((.=), use, zoom)
import Control.Monad (void, unless)
import Data.Bits (shiftR)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector.Unboxed as UV

import Quake
import QuakeState
import QCommon.XCommandT
import qualified Constants
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
          let buf = UV.replicate Constants.conTextSize ' '
          currentLine <- use $ globals.con.cCurrent
          let updatedBuf = fillInBuf oldTotalLines oldWidth currentLine totalLines width tbuf buf 0 0 numLines numChars
          globals.con.cText .= (BC.pack $ UV.toList updatedBuf) -- IMPROVE: performance?

          clearNotify

      totalLines <- use $ globals.con.cTotalLines
      globals.con.cCurrent .= totalLines - 1
      globals.con.cDisplay .= totalLines - 1

        -- IMPROVE: em, can we optimize it? some other approach maybe?
  where fillInBuf :: Int -> Int -> Int -> Int -> Int -> B.ByteString -> UV.Vector Char -> Int -> Int -> Int -> Int -> UV.Vector Char
        fillInBuf oldTotalLines oldWidth currentLine totalLines lineWidth tbuf buf i j maxI maxJ
          | i >= maxI = buf
          | j >= maxJ = fillInBuf oldTotalLines oldWidth currentLine totalLines lineWidth tbuf buf (i + 1) 0 maxI maxJ
          | otherwise =
              let idx = (totalLines - 1 - i) * lineWidth + j
                  idx2 = ((currentLine - i + oldTotalLines) `mod` oldTotalLines) * oldWidth + j
              in fillInBuf oldTotalLines oldWidth currentLine totalLines lineWidth tbuf (buf UV.// [(idx, BC.index tbuf idx2)]) i (j + 1) maxI maxJ

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
