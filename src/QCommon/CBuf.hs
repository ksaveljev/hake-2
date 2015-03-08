{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module QCommon.CBuf where

import Control.Lens (use, (.=))
import Control.Monad (when)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Quake
import QuakeState
import qualified QCommon.SZ as SZ
import qualified QCommon.Com as Com

init :: Quake ()
init = do
    bufData <- use $ globals.cmdTextBuf
    SZ.init (globals.cmdText) bufData (B.length bufData)

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

execute :: Quake ()
execute = do
    globals.aliasCount .= 0

    curSize <- use $ globals.cmdText.sbCurSize
    text <- use $ globals.cmdText.sbData

    when (curSize /= 0) $ doStuff text curSize 0 0

  where doStuff :: B.ByteString -> Int -> Int -> Int -> Quake ()
        doStuff text curSize idx quotes = do
          if | idx == curSize -> do
                 undefined -- TODO
             | BC.index text idx == '"' -> do
                 doStuff text curSize (idx + 1) (quotes + 1)
             | (BC.index text idx == ';' && even quotes) || BC.index text idx == '\n' -> do
                 undefined -- TODO
             | otherwise -> doStuff text curSize (idx + 1) quotes

addText :: B.ByteString -> Quake ()
addText text = do
    let len = B.length text

    curSize <- use $ globals.cmdText.sbCurSize
    maxSize <- use $ globals.cmdText.sbMaxSize

    if curSize + len >= maxSize
      then Com.printf "Cbuf.addText: overflow\n"
      else SZ.write (globals.cmdText) text (B.length text)
