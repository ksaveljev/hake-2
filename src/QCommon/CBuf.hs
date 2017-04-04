{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module QCommon.CBuf where

import Control.Lens (use, (.=), (%=), (-=))
import Control.Monad (when, liftM)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import QCommon.SizeBufT
import Types
import QuakeState
import qualified QCommon.SZ as SZ
import {-# SOURCE #-} qualified QCommon.Com as Com
import {-# SOURCE #-} qualified Game.Cmd as Cmd

initialize :: Quake ()
initialize = do
    bufData <- use $ globals.gCmdTextBuf
    SZ.init (globals.gCmdText) bufData 8192

addEarlyCommands :: Bool -> Quake ()
addEarlyCommands clear = do
    c <- Com.argc
    findAddCommand c 0

  where findAddCommand c i = do
          s <- Com.argv i
          if s /= "+set"
            then when (i + 1 < c) $ findAddCommand c (i + 1)
            else do
              v1 <- Com.argv (i + 1)
              v2 <- Com.argv (i + 2)
              addText $ "set " `B.append` v1 `B.append` " " `B.append` v2 `B.append` "\n"
              when clear $ do
                             Com.clearArgv i
                             Com.clearArgv (i+1)
                             Com.clearArgv (i+2)
              when (i + 3 < c) $ findAddCommand c (i + 3)

addLateCommands :: Quake Bool
addLateCommands = do
    argc <- Com.argc

    -- build the combined string to parse from
    argsLength <- mapM (liftM B.length . Com.argv) [1..argc-1]

    if sum argsLength == 0
      then return False
      else do
        text <- liftM (B.intercalate " ") (mapM Com.argv [1..argc-1])

        -- pull out the commands
        let build = pullOutCommands text (B.length text) 0 ""
            ret = B.length build /= 0

        when ret $ addText build

        return ret

  where pullOutCommands :: B.ByteString -> Int -> Int -> B.ByteString -> B.ByteString
        pullOutCommands txt len idx accum
          | idx >= len = accum
          | txt `BC.index` idx == '+' =
              let command = BC.takeWhile (\ch -> ch /= '+' && ch /= '-') (B.drop (idx + 1) txt)
                  commandLen = B.length command
              in pullOutCommands txt len (idx + commandLen) (accum `B.append` command `B.append` "\n")
          | otherwise = pullOutCommands txt len (idx + 1) accum

execute :: Quake ()
execute = do
    globals.gAliasCount .= 0

    curSize <- use $ globals.gCmdText.sbCurSize
    text <- use $ globals.gCmdText.sbData

    when (curSize /= 0) $ doStuff text curSize 0 0

  where doStuff :: B.ByteString -> Int -> Int -> Int -> Quake ()
        doStuff text curSize idx quotes =
          if | idx == curSize -> do
                 globals.gCmdText.sbCurSize .= 0

                 Cmd.executeString text

                 wait <- use $ globals.gCmdWait

                 when wait $ globals.gCmdWait .= False

             | BC.index text idx == '"' ->
                 doStuff text curSize (idx + 1) (quotes + 1)

             | (BC.index text idx == ';' && even quotes) || BC.index text idx == '\n' -> do
                 let line = B.take idx text -- do not include ';' or '\n'

                 if idx == curSize
                   then globals.gCmdText.sbCurSize .= 0
                   else do
                     globals.gCmdText.sbCurSize -= idx + 1
                     globals.gCmdText.sbData %= B.drop (idx + 1)

                 Cmd.executeString line

                 wait <- use $ globals.gCmdWait

                 if wait
                   -- skip out while text still remains in buffer, leaving
                   -- it for next frame
                   then globals.gCmdWait .= False
                   else do
                     newText <- use $ globals.gCmdText.sbData
                     newCurSize <- use $ globals.gCmdText.sbCurSize
                     when (newCurSize /= 0) $ doStuff newText newCurSize 0 0

             | otherwise -> doStuff text curSize (idx + 1) quotes

addText :: B.ByteString -> Quake ()
addText text = do
    let len = B.length text

    curSize <- use $ globals.gCmdText.sbCurSize
    maxSize <- use $ globals.gCmdText.sbMaxSize

    if curSize + len >= maxSize
      then Com.printf "Cbuf.addText: overflow\n"
      else SZ.write (globals.gCmdText) text (B.length text)

insertText :: B.ByteString -> Quake ()
insertText text = do
    templen <- use $ globals.gCmdText.sbCurSize

    -- copy off an commands still remaining in the exec buffer
    tmp <- if templen /= 0
             then do
               txt <- use $ globals.gCmdText.sbData
               SZ.clear (globals.gCmdText)
               return $ B.take templen txt
             else return ""

    -- add the entire text of the file
    addText text

    when (templen /= 0) $ SZ.write (globals.gCmdText) tmp templen

copyToDefer :: Quake ()
copyToDefer = do
    buf <- use $ globals.gCmdTextBuf
    curSize <- use $ globals.gCmdText.sbCurSize

    globals.gDeferTextBuf .= B.take curSize buf
    globals.gCmdText.sbCurSize .= 0

executeText :: Int -> B.ByteString -> Quake ()
executeText _ _ = io (putStrLn "CBuf.executeText") >> undefined -- TODO

insertFromDefer :: Quake ()
insertFromDefer = do
    buf <- use $ globals.gDeferTextBuf
    insertText buf
    globals.gDeferTextBuf .= ""
