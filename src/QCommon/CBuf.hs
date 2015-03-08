{-# LANGUAGE OverloadedStrings #-}
module QCommon.CBuf where

import Control.Lens (use, (.=))
import Control.Monad (when)

import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as UV

import Quake
import QuakeState
import qualified QCommon.SZ as SZ
import qualified QCommon.Com as Com

init :: Quake ()
init = do
    bufData <- use $ globals.cmdTextBuf
    SZ.init (globals.cmdText) bufData (UV.length bufData)

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

    undefined -- TODO

addText :: B.ByteString -> Quake ()
addText text = do
    let len = B.length text

    curSize <- use $ globals.cmdText.sbCurSize
    maxSize <- use $ globals.cmdText.sbMaxSize

    if curSize + len >= maxSize
      then Com.printf "Cbuf.addText: overflow\n"
      else SZ.write (globals.cmdText) text (B.length text)
