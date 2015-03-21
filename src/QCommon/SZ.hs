{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
module QCommon.SZ where

import Control.Monad (when, unless)
import Control.Lens ((^.), use, (.=))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Quake
import QuakeState
import qualified Constants
import qualified QCommon.Com as Com

init :: QuakeLens SizeBufT -> B.ByteString -> Int -> Quake ()
init bufLens bufData len =
    bufLens .= newSizeBufT { _sbData = bufData, _sbMaxSize = len }

clear :: QuakeLens SizeBufT -> Quake ()
clear bufLens = do
    bufLens.sbCurSize .= 0
    bufLens.sbOverflowed .= False

-- ask for the pointer using sizebuf_t.cursize (RST)
getSpace :: QuakeLens SizeBufT -> Int -> Quake Int
getSpace bufLens len = do
    buf <- use bufLens

    when (buf^.sbCurSize + len > buf^.sbMaxSize) $
      do
        unless (buf^.sbAllowOverflow) $ Com.comError Constants.errFatal "SZ_GetSpace: overflow without allowoverflow set"
        when (len > buf^.sbMaxSize) $ Com.comError Constants.errFatal ("SZ_GetSpace: " `B.append` BC.pack (show len) `B.append` " is > full buffer size") -- IMPROVE: convert Int to ByteString using binary package?

        Com.printf "SZ.getSpace: overflow\n"
        clear bufLens
        bufLens.sbOverflowed .= True

    let oldsize = buf^.sbCurSize
    bufLens.sbCurSize .= oldsize + len

    return oldsize

write :: QuakeLens SizeBufT -> B.ByteString -> Int -> Quake ()
write bufLens bufData len = do
    idx <- getSpace bufLens len
    oldData <- use $ bufLens.sbData
    let updatedData = B.take idx oldData `B.append` bufData `B.append` B.drop (idx + len) oldData
    bufLens.sbData .= updatedData

print :: QuakeLens SizeBufT -> B.ByteString -> Quake ()
print _ _ = io (putStrLn "SZ.print") >> undefined -- TODO
