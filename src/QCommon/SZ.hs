{-# LANGUAGE Rank2Types #-}
module QCommon.SZ
    ( clear
    , initialize
    , printSB
    , write
    ) where

import           Control.Lens     (ASetter', Lens', Traversal', preuse, (.=), (%=), (^.), (+=), (&), (.~))
import           Control.Monad    (when, unless)
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
printSB = error "SZ.print" -- TODO
