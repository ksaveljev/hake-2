{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
module QCommon.SZ where

import Control.Monad (when, unless)
import Control.Lens ((^.), use, (.=))
import qualified Data.ByteString as B

import Quake
import QuakeState
import qualified QCommon.Com as Com

init :: SizeBufTLens -> B.ByteString -> Int -> Quake ()
init bufLens bufData len =
    bufLens .= newSizeBufT { _sbData = bufData, _sbMaxSize = len }

clear :: SizeBufTLens -> Quake ()
clear bufLens = do
    bufLens.sbCurSize .= 0
    bufLens.sbOverflowed .= False

getSpace :: SizeBufTLens -> Int -> Quake Int
getSpace bufLens len = do
    buf <- use bufLens

    when (buf^.sbCurSize + len > buf^.sbMaxSize) $
      do
        unless (buf^.sbAllowOverflow) $ Com.comError undefined undefined -- TODO
        when (len > buf^.sbMaxSize) $ Com.comError undefined undefined -- TODO

        Com.printf "SZ.getSpace: overflow\n"
        clear bufLens
        bufLens.sbOverflowed .= True

    let oldsize = buf^.sbCurSize
    bufLens.sbCurSize .= oldsize + len

    return oldsize

write :: SizeBufTLens -> B.ByteString -> Int -> Quake ()
write bufLens bufData len = do
    idx <- getSpace bufLens len
    oldData <- use $ bufLens.sbData
    let updatedData = B.take idx oldData `B.append` bufData `B.append` B.drop (idx + len) oldData
    bufLens.sbData .= updatedData
