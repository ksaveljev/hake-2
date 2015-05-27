{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
module QCommon.MSG where

import Control.Lens (ASetter', Traversal', Lens', (.=), use, (^.), (+=))
import Control.Monad (when, liftM, unless)
import Data.Bits ((.&.), shiftR, shiftL, (.|.))
import Data.Int (Int8, Int32)
import Data.Monoid (mempty, mappend)
import Data.Word (Word8)
import Linear (V3(..), _x, _y, _z, dot)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

import Quake
import QuakeState
import qualified Constants
import qualified QCommon.Com as Com
import qualified QCommon.SZ as SZ

-- IMPROVE: use binary package for conversion to ByteString?

writeCharI :: Traversal' QuakeState SizeBufT -> Int -> Quake ()
writeCharI = writeByteI

writeCharF :: Traversal' QuakeState SizeBufT -> Float -> Quake ()
writeCharF = writeByteF

writeByteI :: Traversal' QuakeState SizeBufT -> Int -> Quake ()
writeByteI sizeBufLens c = do
    SZ.write sizeBufLens (B.pack [fromIntegral (c .&. 0xFF)]) 1

writeByteF :: Traversal' QuakeState SizeBufT -> Float -> Quake ()
writeByteF sizeBufLens c = do
    writeByteI sizeBufLens (truncate c)

writeShort :: Traversal' QuakeState SizeBufT -> Int -> Quake ()
writeShort sizeBufLens c = do
    let a :: Word8 = fromIntegral (c .&. 0xFF)
        b :: Word8 = fromIntegral ((c `shiftR` 8) .&. 0xFF)
    SZ.write sizeBufLens (B.pack [a, b]) 2

writeInt :: Traversal' QuakeState SizeBufT -> Int -> Quake ()
writeInt sizeBufLens v = do
    let a :: Word8 = fromIntegral (v .&. 0xFF)
        b :: Word8 = fromIntegral ((v `shiftR` 8) .&. 0xFF)
        c :: Word8 = fromIntegral ((v `shiftR` 16) .&. 0xFF)
        d :: Word8 = fromIntegral ((v `shiftR` 24) .&. 0xFF)
    SZ.write sizeBufLens (B.pack [a, b, c, d]) 4

writeLong :: Traversal' QuakeState SizeBufT -> Int -> Quake ()
writeLong sizeBufLens v = writeInt sizeBufLens v

writeFloat :: ASetter' QuakeState SizeBufT -> Float -> Quake ()
writeFloat _ _ = io (putStrLn "MSG.writeFloat") >> undefined -- TODO

writeString :: Traversal' QuakeState SizeBufT -> B.ByteString -> Quake ()
writeString sizeBufLens s = do
    SZ.write sizeBufLens s (B.length s)
    writeByteI sizeBufLens 0

writeCoord :: Traversal' QuakeState SizeBufT -> Float -> Quake ()
writeCoord sizeBufLens f =
    writeShort sizeBufLens (truncate $ f * 8)

writePos :: Traversal' QuakeState SizeBufT -> V3 Float -> Quake ()
writePos sizeBufLens pos = do
    writeShort sizeBufLens (truncate ((pos^._x) * 8))
    writeShort sizeBufLens (truncate ((pos^._y) * 8))
    writeShort sizeBufLens (truncate ((pos^._z) * 8))

writeDir :: Traversal' QuakeState SizeBufT -> V3 Float -> Quake ()
writeDir sizeBufLens dir = do
    -- do we need this?
    {-
        if (dir == null) {
            WriteByte(sb, 0);
            return;
        }
    -}
    let best = calcBest 0 0 0 Constants.numVertexNormals
    writeByteI sizeBufLens (fromIntegral best)

  where calcBest :: Float -> Int -> Int -> Int -> Int
        calcBest bestd best idx maxIdx
          | idx >= maxIdx = best
          | otherwise =
              let d = dot dir (Constants.byteDirs V.! idx)
              in if d > bestd
                   then calcBest d idx (idx + 1) maxIdx
                   else calcBest bestd best (idx + 1) maxIdx

writeAngle :: ASetter' QuakeState SizeBufT -> Float -> Quake ()
writeAngle _ _ = io (putStrLn "MSG.writeAngle") >> undefined -- TODO

writeAngle16 :: ASetter' QuakeState SizeBufT -> Float -> Quake ()
writeAngle16 _ _ = io (putStrLn "MSG.writeAngle16") >> undefined -- TODO

writeDeltaUserCmd :: Traversal' QuakeState SizeBufT -> UserCmdT -> UserCmdT -> Quake ()
writeDeltaUserCmd sizeBufLens from cmd = do
    -- send the movement message
    let a = if (cmd^.ucAngles._x) /= (from^.ucAngles._x) then Constants.cmAngle1 else 0
        b = if (cmd^.ucAngles._y) /= (from^.ucAngles._y) then Constants.cmAngle2 else 0
        c = if (cmd^.ucAngles._z) /= (from^.ucAngles._z) then Constants.cmAngle3 else 0
        d = if (cmd^.ucForwardMove) /= (from^.ucForwardMove) then Constants.cmForward else 0
        e = if (cmd^.ucSideMove) /= (from^.ucSideMove) then Constants.cmSide else 0
        f = if (cmd^.ucUpMove) /= (from^.ucUpMove) then Constants.cmUp else 0
        g = if (cmd^.ucButtons) /= (from^.ucButtons) then Constants.cmButtons else 0
        h = if (cmd^.ucImpulse) /= (from^.ucImpulse) then Constants.cmImpulse else 0

        bits = a .|. b .|. c .|. d .|. e .|. f .|. g .|. h

    writeByteI sizeBufLens (fromIntegral bits)

    when (bits .&. Constants.cmAngle1 /= 0) $
      writeShort sizeBufLens (fromIntegral $ cmd^.ucAngles._x)
    when (bits .&. Constants.cmAngle2 /= 0) $
      writeShort sizeBufLens (fromIntegral $ cmd^.ucAngles._y)
    when (bits .&. Constants.cmAngle3 /= 0) $
      writeShort sizeBufLens (fromIntegral $ cmd^.ucAngles._z)

    when (bits .&. Constants.cmForward /= 0) $
      writeShort sizeBufLens (fromIntegral $ cmd^.ucForwardMove)
    when (bits .&. Constants.cmSide /= 0) $
      writeShort sizeBufLens (fromIntegral $ cmd^.ucSideMove)
    when (bits .&. Constants.cmUp /= 0) $
      writeShort sizeBufLens (fromIntegral $ cmd^.ucUpMove)

    when (bits .&. Constants.cmButtons /= 0) $
      writeByteI sizeBufLens (fromIntegral $ cmd^.ucButtons)
    when (bits .&. Constants.cmImpulse /= 0) $
      writeByteI sizeBufLens (fromIntegral $ cmd^.ucImpulse)

    writeByteI sizeBufLens (fromIntegral $ cmd^.ucMsec)
    writeByteI sizeBufLens (fromIntegral $ cmd^.ucLightLevel)

{-
- ================== WriteDeltaEntity ==================
- 
- Writes part of a packetentities message. Can delta from either a baseline
- or a previous packet_entity
-}
writeDeltaEntity :: EntityStateT -> EntityStateT -> Traversal' QuakeState SizeBufT -> Bool -> Bool -> Quake ()
writeDeltaEntity from to sizeBufLens force newEntity = do
    when ((to^.esNumber) == 0) $
      Com.comError Constants.errFatal "Unset entity number"

    when ((to^.esNumber) >= Constants.maxEdicts) $
      Com.comError Constants.errFatal "Entity number >= MAX_EDICTS"

    -- send an update
    let a = if (to^.esNumber) >= 256 then Constants.uNumber16 else 0 -- number8 is implicit otherwise
        b = if (to^.esOrigin._x) /= (from^.esOrigin._x) then Constants.uOrigin1 else 0
        c = if (to^.esOrigin._y) /= (from^.esOrigin._y) then Constants.uOrigin2 else 0
        d = if (to^.esOrigin._z) /= (from^.esOrigin._z) then Constants.uOrigin3 else 0
        e = if (to^.esAngles._x) /= (from^.esAngles._x) then Constants.uAngle1 else 0
        f = if (to^.esAngles._y) /= (from^.esAngles._y) then Constants.uAngle2 else 0
        g = if (to^.esAngles._z) /= (from^.esAngles._z) then Constants.uAngle3 else 0
        h = if (to^.esSkinNum) /= (from^.esSkinNum)
              then if | (to^.esSkinNum) < 256 -> Constants.uSkin8
                      | (to^.esSkinNum) < 0x10000 -> Constants.uSkin16
                      | otherwise -> Constants.uSkin8 .|. Constants.uSkin16
              else 0
        i = if (to^.esFrame) /= (from^.esFrame)
              then if (to^.esFrame) < 256
                     then Constants.uFrame8
                     else Constants.uFrame16
              else 0
        j = if (to^.esEffects) /= (from^.esEffects)
              then if | (to^.esEffects) < 256 -> Constants.uEffects8
                      | (to^.esEffects) < 0x8000 -> Constants.uEffects16
                      | otherwise -> Constants.uEffects8 .|. Constants.uEffects16
              else 0
        k = if (to^.esRenderFx) /= (from^.esRenderFx)
              then if | (to^.esRenderFx) < 256 -> Constants.uRenderFx8
                      | (to^.esRenderFx) < 0x8000 -> Constants.uRenderFx16
                      | otherwise -> Constants.uRenderFx8 .|. Constants.uRenderFx16
              else 0
        l = if (to^.esSolid) /= (from^.esSolid) then Constants.uSolid else 0
        -- event is not delta compressed, just 0 compressed
        m = if (to^.esEvent) /= 0 then Constants.uEvent else 0
        n = if (to^.esModelIndex) /= (from^.esModelIndex) then Constants.uModel else 0
        o = if (to^.esModelIndex2) /= (from^.esModelIndex2) then Constants.uModel2 else 0
        p = if (to^.esModelIndex3) /= (from^.esModelIndex3) then Constants.uModel3 else 0
        q = if (to^.esModelIndex4) /= (from^.esModelIndex4) then Constants.uModel4 else 0
        r = if (to^.esSound) /= (from^.esSound) then Constants.uSound else 0
        s = if newEntity || (to^.esRenderFx) .&. Constants.rfBeam /= 0 then Constants.uOldOrigin else 0

        bits = a .|. b .|. c .|. d .|. e .|. f .|. g .|. h .|. i .|. j .|. k .|. l .|. m .|. n .|. o .|. p .|. q .|. r .|. s

    -- write the message
    unless (bits == 0 && not force) $ do
      let finalBits = if | bits .&. 0xFF000000 /= 0 -> bits .|. Constants.uMoreBits3 .|. Constants.uMoreBits2 .|. Constants.uMoreBits1
                         | bits .&. 0x00FF0000 /= 0 -> bits .|. Constants.uMoreBits2 .|. Constants.uMoreBits1
                         | bits .&. 0x0000FF00 /= 0 -> bits .|. Constants.uMoreBits1
                         | otherwise -> bits

      writeByteI sizeBufLens (finalBits .&. 255)

      if | finalBits .&. 0xFF000000 /= 0 -> do
             writeByteI sizeBufLens ((finalBits `shiftR`  8) .&. 0xFF)
             writeByteI sizeBufLens ((finalBits `shiftR` 16) .&. 0xFF)
             writeByteI sizeBufLens ((finalBits `shiftR` 24) .&. 0xFF)
         | finalBits .&. 0x00FF0000 /= 0 -> do
             writeByteI sizeBufLens ((finalBits `shiftR`  8) .&. 0xFF)
             writeByteI sizeBufLens ((finalBits `shiftR` 16) .&. 0xFF)
         | finalBits .&. 0x0000FF00 /= 0 -> do
             writeByteI sizeBufLens ((finalBits `shiftR` 8) .&. 0xFF)
         | otherwise -> return ()

      if finalBits .&. Constants.uNumber16 /= 0
        then writeShort sizeBufLens (to^.esNumber)
        else writeByteI sizeBufLens (to^.esNumber)

      when (finalBits .&. Constants.uModel /= 0) $
        writeByteI sizeBufLens (to^.esModelIndex)
      when (finalBits .&. Constants.uModel2 /= 0) $
        writeByteI sizeBufLens (to^.esModelIndex2)
      when (finalBits .&. Constants.uModel3 /= 0) $
        writeByteI sizeBufLens (to^.esModelIndex3)
      when (finalBits .&. Constants.uModel4 /= 0) $
        writeByteI sizeBufLens (to^.esModelIndex4)

      when (finalBits .&. Constants.uFrame8 /= 0) $
        writeByteI sizeBufLens (to^.esFrame)
      when (finalBits .&. Constants.uFrame16 /= 0) $
        writeShort sizeBufLens (to^.esFrame)

      if | finalBits .&. Constants.uSkin8 /= 0 && finalBits .&. Constants.uSkin16 /= 0 -> -- used for laser colors
             writeInt sizeBufLens (to^.esSkinNum)
         | finalBits .&. Constants.uSkin8 /= 0 ->
             writeByteI sizeBufLens (to^.esSkinNum)
         | finalBits .&. Constants.uSkin16 /= 0 ->
             writeShort sizeBufLens (to^.esSkinNum)
         | otherwise -> return ()

      if | finalBits .&. (Constants.uEffects8 .|. Constants.uEffects16) == (Constants.uEffects8 .|. Constants.uEffects16) ->
             writeInt sizeBufLens (to^.esEffects)
         | finalBits .&. Constants.uEffects8 /= 0 ->
             writeByteI sizeBufLens (to^.esEffects)
         | finalBits .&. Constants.uEffects16 /= 0 ->
             writeShort sizeBufLens (to^.esEffects)
         | otherwise -> return ()


      if | finalBits .&. (Constants.uRenderFx8 .|. Constants.uRenderFx16) == (Constants.uRenderFx8 .|. Constants.uRenderFx16) ->
             writeInt sizeBufLens (to^.esRenderFx)
         | finalBits .&. Constants.uRenderFx8 /= 0 ->
             writeByteI sizeBufLens (to^.esRenderFx)
         | finalBits .&. Constants.uRenderFx16 /= 0 ->
             writeShort sizeBufLens (to^.esRenderFx)
         | otherwise -> return ()

      when (finalBits .&. Constants.uOrigin1 /= 0) $
        writeCoord sizeBufLens (to^.esOrigin._x)
      when (finalBits .&. Constants.uOrigin2 /= 0) $
        writeCoord sizeBufLens (to^.esOrigin._y)
      when (finalBits .&. Constants.uOrigin3 /= 0) $
        writeCoord sizeBufLens (to^.esOrigin._z)

      when (finalBits .&. Constants.uAngle1 /= 0) $
        writeAngle sizeBufLens (to^.esAngles._x)
      when (finalBits .&. Constants.uAngle2 /= 0) $
        writeAngle sizeBufLens (to^.esAngles._y)
      when (finalBits .&. Constants.uAngle3 /= 0) $
        writeAngle sizeBufLens (to^.esAngles._z)

      when (finalBits .&. Constants.uOldOrigin /= 0) $ do
        writeCoord sizeBufLens (to^.esOldOrigin._x)
        writeCoord sizeBufLens (to^.esOldOrigin._y)
        writeCoord sizeBufLens (to^.esOldOrigin._z)

      when (finalBits .&. Constants.uSound /= 0) $
        writeByteI sizeBufLens (to^.esSound)
      when (finalBits .&. Constants.uEvent /= 0) $
        writeByteI sizeBufLens (to^.esEvent)
      when (finalBits .&. Constants.uSolid /= 0) $
        writeShort sizeBufLens (to^.esSolid)

--
-- reading functions
--

beginReading :: Lens' QuakeState SizeBufT -> Quake ()
beginReading sizeBufLens =
    sizeBufLens.sbReadCount .= 0

-- IMPROVE: convert bytestring to int using binary package?
readLong :: Lens' QuakeState SizeBufT -> Quake Int
readLong sizeBufLens = do
    sizeBuf <- use $ sizeBufLens

    if (sizeBuf^.sbReadCount) + 4 > (sizeBuf^.sbCurSize)
      then do
        Com.printf "buffer underrun in ReadLong!"
        return (-1)
      else do
        let buf = sizeBuf^.sbData
            readCount = sizeBuf^.sbReadCount
            a :: Int32 = fromIntegral $ B.index buf readCount
            b :: Int32 = fromIntegral $ B.index buf (readCount + 1)
            c :: Int32 = fromIntegral $ B.index buf (readCount + 2)
            d :: Int32 = fromIntegral $ B.index buf (readCount + 3)
            result = a .|. (b `shiftL` 8) .|. (c `shiftL` 16) .|. (d `shiftL` 24)
        sizeBufLens.sbReadCount += 4
        return $ fromIntegral result 

readByte :: Lens' QuakeState SizeBufT -> Quake Int
readByte sizeBufLens = do
    sizeBuf <- use sizeBufLens

    let c = if (sizeBuf^.sbReadCount) + 1 > (sizeBuf^.sbCurSize)
              then (-1)
              else fromIntegral $ (sizeBuf^.sbData) `B.index` (sizeBuf^.sbReadCount)

    sizeBufLens.sbReadCount += 1

    return c

readShort :: Lens' QuakeState SizeBufT -> Quake Int
readShort sizeBufLens = do
    sizeBuf <- use sizeBufLens

    if (sizeBuf^.sbReadCount) + 2 > (sizeBuf^.sbCurSize)
      then do
        Com.printf "buffer underrun in ReadLong!"
        return (-1)
      else do
        let buf = sizeBuf^.sbData
            readCount = sizeBuf^.sbReadCount
            a :: Int = fromIntegral $ B.index buf readCount
            b :: Int = fromIntegral $ B.index buf (readCount + 1)
            result = a .|. (b `shiftL` 8)
        sizeBufLens.sbReadCount += 2
        return result 

readChar :: Lens' QuakeState SizeBufT -> Quake Int8
readChar sizeBufLens = do
    msgRead <- use sizeBufLens

    let c = if (msgRead^.sbReadCount) + 1 > (msgRead^.sbCurSize)
              then (-1)
              else fromIntegral $ B.index (msgRead^.sbData) (msgRead^.sbReadCount)

    sizeBufLens.sbReadCount += 1

    return c

readStringLine :: Lens' QuakeState SizeBufT -> Quake B.ByteString
readStringLine sizeBufLens = do
    ret <- readStr 0 mempty
    let trimmedRet = B.reverse . BC.dropWhile (<= ' ') . B.reverse $ ret
    Com.dprintf $ "MSG.ReadStringLine:[" `B.append` trimmedRet `B.append` "]\n"
    return trimmedRet

  where readStr :: Int -> BB.Builder -> Quake B.ByteString
        readStr idx acc
          | idx >= 2047 = return (BL.toStrict $ BB.toLazyByteString acc)
          | otherwise = do
              c <- readChar sizeBufLens
              if c == -1 || c == 0 || c == 0x0A
                then return (BL.toStrict $ BB.toLazyByteString acc)
                else readStr (idx + 1) (acc `mappend` BB.int8 c)

readString :: Lens' QuakeState SizeBufT -> Quake B.ByteString
readString sizeBufLens = do
    buildString 0 mempty

  where buildString :: Int -> BB.Builder -> Quake B.ByteString
        buildString len acc
          | len >= 2047 = return (BL.toStrict $ BB.toLazyByteString acc)
          | otherwise = do
              c <- readByte sizeBufLens

              if c == -1 || c == 0
                then return (BL.toStrict $ BB.toLazyByteString acc)
                else buildString (len + 1) (acc `mappend` (BB.word8 $ fromIntegral c))

readDeltaUserCmd :: Lens' QuakeState SizeBufT -> UserCmdT -> Quake UserCmdT
readDeltaUserCmd sizeBufLens from = do
    let move = from

    bits <- liftM fromIntegral $ readByte sizeBufLens

    -- read current angles
    a1 <- if bits .&. Constants.cmAngle1 /= 0 then liftM fromIntegral (readShort sizeBufLens) else return (from^.ucAngles._x)
    a2 <- if bits .&. Constants.cmAngle2 /= 0 then liftM fromIntegral (readShort sizeBufLens) else return (from^.ucAngles._y)
    a3 <- if bits .&. Constants.cmAngle3 /= 0 then liftM fromIntegral (readShort sizeBufLens) else return (from^.ucAngles._z)

    -- read movement
    forward <- if bits .&. Constants.cmForward /= 0 then liftM fromIntegral (readShort sizeBufLens) else return (from^.ucForwardMove)
    side <- if bits .&. Constants.cmSide /= 0 then liftM fromIntegral (readShort sizeBufLens) else return (from^.ucSideMove)
    up <- if bits .&. Constants.cmUp /= 0 then liftM fromIntegral (readShort sizeBufLens) else return (from^.ucUpMove)

    -- read buttons
    buttons <- if bits .&. Constants.cmButtons /= 0 then liftM fromIntegral (readByte sizeBufLens) else return (from^.ucButtons)
    impulse <- if bits .&. Constants.cmImpulse /= 0 then liftM fromIntegral (readByte sizeBufLens) else return (from^.ucImpulse)

    -- read time to run command
    msec <- readByte sizeBufLens

    -- read the light level
    lightLevel <- readByte sizeBufLens

    return move { _ucAngles      = V3 a1 a2 a3
                , _ucForwardMove = forward
                , _ucSideMove    = side
                , _ucUpMove      = up
                , _ucButtons     = buttons
                , _ucImpulse     = impulse
                , _ucMsec        = fromIntegral msec
                , _ucLightLevel  = fromIntegral lightLevel
                }
