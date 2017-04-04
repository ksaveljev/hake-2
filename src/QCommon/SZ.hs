{-# LANGUAGE Rank2Types #-}
module QCommon.SZ where

import Control.Monad (when, unless)
import Control.Lens (Lens', Traversal', preuse, (&), (.~), (%=), (+=), (^.), use, (.=))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import QCommon.SizeBufT
import Types
import QuakeState
import qualified Constants
import {-# SOURCE #-} qualified QCommon.Com as Com
import Util.Binary (encode)

init :: Traversal' QuakeState SizeBufT -> B.ByteString -> Int -> Quake ()
init bufLens bufData maxLen =
    bufLens .= sizeBuf
  where
    sizeBuf = newSizeBufT & sbData    .~ bufData
                          & sbMaxSize .~ maxLen

clear :: Traversal' QuakeState SizeBufT -> Quake ()
clear bufLens =
    bufLens %= (\v -> v & sbCurSize    .~ 0
                        & sbData       .~ B.empty
                        & sbOverflowed .~ False)

-- ask for the pointer using sizebuf_t.cursize (RST)
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

write :: Traversal' QuakeState SizeBufT -> B.ByteString -> Int -> Quake ()
write bufLens bufData len =
  do idx <- getSpace bufLens len
     bufLens.sbData %= (\old -> (B.take idx old) `B.append` bufData)

print :: Lens' QuakeState SizeBufT -> B.ByteString -> Quake ()
print sizeBufLens str = do
    Com.dprintf $ "SZ.print():<" `B.append` str `B.append` ">\n"
    let len = B.length str + 1

    use sizeBufLens >>= \buf -> do
      if (buf^.sbCurSize) /= 0
        then do
          if (buf^.sbData) `B.index` ((buf^.sbCurSize) - 1) /= 0
            then do
              -- no trailing 0
              idx <- getSpace sizeBufLens len
              sizeBufLens.sbData .= B.take idx (buf^.sbData) `B.append` str `B.append` "\0"
            else do
              -- write over trailing 0
              idx <- getSpace sizeBufLens len
              sizeBufLens.sbData .= B.take (idx - 1) (buf^.sbData) `B.append` str `B.append` "\0"
        else do
          _ <- getSpace sizeBufLens len
          sizeBufLens.sbData .= str `B.append` "\0"
