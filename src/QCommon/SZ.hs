{-# LANGUAGE Rank2Types #-}
module QCommon.SZ
    ( clear
    , initialize
    , printSB
    , write
    ) where

import           Control.Lens     (ASetter', Lens', Traversal', use, preuse, (.=), (%=), (^.), (+=), (&), (.~))
import           Control.Monad    (when, unless, void)
import qualified Data.ByteString  as B

import           QCommon.SizeBufT
import           Types
import           Util.Binary      (encode)

import {-# SOURCE #-} qualified QCommon.Com as Com

initialize :: Traversal' QuakeState SizeBufT -> B.ByteString -> Int -> Quake ()
initialize bufLens bufData maxLen =
    bufLens .= sizeBuf
  where
    sizeBuf = newSizeBufT & sbData .~ bufData
                          & sbMaxSize .~ maxLen

write :: Traversal' QuakeState SizeBufT -> B.ByteString -> Int -> Quake ()
write bufLens bufData len = do
    idx <- getSpace bufLens len
    bufLens.sbData %= (\old -> B.take idx old `B.append` bufData)


getSpace :: Traversal' QuakeState SizeBufT -> Int -> Quake Int
getSpace bufLens len = do
    buf <- preuse bufLens
    maybe getSpaceError proceedGettingSpace buf
  where
    proceedGettingSpace buf = do
        when (isOverflow buf) (overflow buf)
        bufLens.sbCurSize += len
        return (buf^.sbCurSize)
    getSpaceError = do
        Com.fatalError "getSpace for non existing SizeBufT"
        return 0 -- to make compiler happy
    isOverflow buf = buf^.sbCurSize + len > buf^.sbMaxSize
    overflow buf = do
        checkOverflowAllowed buf
        checkLen buf
        Com.printf "SZ.getSpace: overflow\n"
        clear bufLens
        bufLens.sbOverflowed .= True
    checkOverflowAllowed buf =
        unless (buf^.sbAllowOverflow) $
            Com.fatalError "SZ_GetSpace: overflow without allowoverflow set"
    checkLen buf =
        when (len > buf^.sbMaxSize) $
            Com.fatalError (B.concat ["SZ_GetSpace: ", encode len, " is > full buffer size"])

clear :: ASetter' QuakeState SizeBufT -> Quake ()
clear bufLens =
    bufLens %= (\v -> v & sbCurSize .~ 0
                        & sbData .~ B.empty
                        & sbOverflowed .~ False)

printSB :: Lens' QuakeState SizeBufT -> B.ByteString -> Quake ()
printSB sizeBufLens str = do
    Com.dprintf (B.concat ["SZ.print():<", str, ">\n"])
    let len = B.length str + 1
    buf <- use sizeBufLens
    doPrint buf len
  where
    doPrint buf len
        | (buf^.sbCurSize) /= 0 && ((buf^.sbData) `B.index` ((buf^.sbCurSize) - 1)) /= 0 = do
            -- no trailing 0
            idx <- getSpace sizeBufLens len
            sizeBufLens.sbData .= B.concat [B.take idx (buf^.sbData), str, "\0"]
        | (buf^.sbCurSize) /= 0 = do
            -- write over trailing 0
            idx <- getSpace sizeBufLens len
            sizeBufLens.sbData .= B.concat [B.take (idx - 1) (buf^.sbData), str, "\0"]
        | otherwise = do
            void (getSpace sizeBufLens len)
            sizeBufLens.sbData .= str `B.append` "\0"
