{-# LANGUAGE Rank2Types #-}
module QCommon.MSG
    ( beginReading
    , readAngle
    , readAngle16
    , readByte
    , readChar
    , readCoord
    , readData
    , readDeltaUserCmd
    , readDir
    , readLong
    , readPos
    , readShort
    , readString
    , readStringLine
    , writeAngle
    , writeAngle16
    , writeByteF
    , writeByteI
    , writeCharF
    , writeCharI
    , writeDeltaEntity
    , writeDeltaUserCmd
    , writeDir
    , writeInt
    , writeLong
    , writePos
    , writeShort
    , writeString
    ) where

import           Control.Lens            (Traversal', Lens', use, (^.), (.=), (+=), (%=))
import           Control.Monad           (when, unless)
import           Data.Bits               (shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString         as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8   as BC
import qualified Data.ByteString.Lazy    as BL
import           Data.Int                (Int8, Int16, Int32)
import qualified Data.Vector             as V
import qualified Data.Vector.Unboxed     as UV
import           Data.Word               (Word8, Word16, Word32)
import           Linear                  (V3(..), dot, _x, _y, _z)

import qualified Constants
import           Game.EntityStateT
import           Game.UserCmdT
import           QCommon.SizeBufT
import qualified QCommon.SZ              as SZ
import           Types
import qualified Util.Math3D             as Math3D

import {-# SOURCE #-} qualified QCommon.Com as Com

writeByteI :: Traversal' QuakeState SizeBufT -> Int -> Quake ()
writeByteI sizeBufLens c = SZ.write sizeBufLens (B.pack [c']) 1
  where
    c' = fromIntegral (c .&. 0xFF) :: Word8

writeByteF :: Traversal' QuakeState SizeBufT -> Float -> Quake ()
writeByteF sizeBufLens c = writeByteI sizeBufLens (truncate c)

writeCharI :: Traversal' QuakeState SizeBufT -> Int -> Quake ()
writeCharI = writeByteI

writeCharF :: Traversal' QuakeState SizeBufT -> Float -> Quake ()
writeCharF = writeByteF

writeInt :: Traversal' QuakeState SizeBufT -> Int -> Quake ()
writeInt sizeBufLens v =  do -- IMPROVE ?
    let v' = fromIntegral v :: Word32
        a = fromIntegral (v' .&. 0xFF) :: Word8
        b = fromIntegral ((v' `shiftR` 8) .&. 0xFF) :: Word8
        c = fromIntegral ((v' `shiftR` 16) .&. 0xFF) :: Word8
        d = fromIntegral ((v' `shiftR` 24) .&. 0xFF) :: Word8
    SZ.write sizeBufLens (B.pack [a, b, c, d]) 4

writeShort :: Traversal' QuakeState SizeBufT -> Int -> Quake ()
writeShort sizeBufLens c = do
    let c' = fromIntegral c :: Word32
        a = fromIntegral (c' .&. 0xFF) :: Word8
        b = fromIntegral ((c' `shiftR` 8) .&. 0xFF) :: Word8
    SZ.write sizeBufLens (B.pack [a, b]) 2

writeLong :: Traversal' QuakeState SizeBufT -> Int -> Quake ()
writeLong = writeInt

writeString :: Traversal' QuakeState SizeBufT -> B.ByteString -> Quake ()
writeString sizeBufLens str = do
    SZ.write sizeBufLens str (B.length str)
    writeByteI sizeBufLens 0

writePos :: Traversal' QuakeState SizeBufT -> V3 Float -> Quake ()
writePos sizeBufLens pos = do
    writeShort sizeBufLens (truncate ((pos^._x) * 8))
    writeShort sizeBufLens (truncate ((pos^._y) * 8))
    writeShort sizeBufLens (truncate ((pos^._z) * 8))

writeCoord :: Traversal' QuakeState SizeBufT -> Float -> Quake ()
writeCoord sizeBufLens f = writeShort sizeBufLens (truncate (f * 8))

writeAngle :: Traversal' QuakeState SizeBufT -> Float -> Quake ()
writeAngle sizeBufLens f = writeByteI sizeBufLens ((truncate (f * 256 / 360)) .&. 255)

writeAngle16 :: Traversal' QuakeState SizeBufT -> Float -> Quake ()
writeAngle16 sizeBufLens f = writeShort sizeBufLens (fromIntegral (Math3D.angleToShort f))

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
  where
    calcBest bestd best idx maxIdx
        | idx >= maxIdx = best
        | otherwise =
            let d = dot dir (Constants.byteDirs V.! idx)
            in if d > bestd
                   then calcBest d idx (idx + 1) maxIdx
                   else calcBest bestd best (idx + 1) maxIdx

beginReading :: Lens' QuakeState SizeBufT -> Quake ()
beginReading sizeBufLens = sizeBufLens.sbReadCount .= 0

readByte :: Lens' QuakeState SizeBufT -> Quake Int
readByte sizeBufLens = doReadByte sizeBufLens =<< use sizeBufLens

doReadByte :: Lens' QuakeState SizeBufT -> SizeBufT -> Quake Int
doReadByte sizeBufLens sizeBuf = do
    sizeBufLens.sbReadCount += 1
    return c
  where
    c | (sizeBuf^.sbReadCount) + 1 > (sizeBuf^.sbCurSize) = -1
      | otherwise = fromIntegral ((sizeBuf^.sbData) `B.index` (sizeBuf^.sbReadCount))

readShort :: Lens' QuakeState SizeBufT -> Quake Int
readShort sizeBufLens = doReadShort sizeBufLens =<< use sizeBufLens

doReadShort :: Lens' QuakeState SizeBufT -> SizeBufT -> Quake Int
doReadShort sizeBufLens sizeBuf
    | (sizeBuf^.sbReadCount) + 2 > (sizeBuf^.sbCurSize) = do
        Com.printf "buffer underrun in ReadShort!"
        return (-1)
    | otherwise = do
        sizeBufLens.sbReadCount += 2
        return (fromIntegral result)
  where
    buf = sizeBuf^.sbData
    readCount = sizeBuf^.sbReadCount
    a = B.index buf readCount
    b = B.index buf (readCount + 1)
    b' = fromIntegral b `shiftL` 8 :: Word16
    result = fromIntegral (fromIntegral a .|. b') :: Int16

readLong :: Lens' QuakeState SizeBufT -> Quake Int
readLong sizeBufLens = doReadLong sizeBufLens =<< use sizeBufLens

doReadLong :: Lens' QuakeState SizeBufT -> SizeBufT -> Quake Int
doReadLong sizeBufLens sizeBuf
    | (sizeBuf^.sbReadCount) + 4 > (sizeBuf^.sbCurSize) = do
        Com.printf "buffer underrun in ReadLong!"
        return (-1)
    | otherwise = do
        sizeBufLens.sbReadCount += 4
        return (fromIntegral result )
  where
    buf = sizeBuf^.sbData
    readCount = sizeBuf^.sbReadCount
    a = fromIntegral (B.index buf readCount) :: Word32
    b = fromIntegral (B.index buf (readCount + 1)) :: Word32
    c = fromIntegral (B.index buf (readCount + 2)) :: Word32
    d = fromIntegral (B.index buf (readCount + 3)) :: Word32
    result = fromIntegral (a .|. (b `shiftL` 8) .|. (c `shiftL` 16) .|. (d `shiftL` 24)) :: Int32

readStringLine :: Lens' QuakeState SizeBufT -> Quake B.ByteString
readStringLine sizeBufLens = do
    str <- fmap trim (readStr sizeBufLens 0 mempty)
    Com.dprintf (B.concat ["MSG.ReadStringLine:[", str, "]\n"])
    return str
  where
    trim = B.reverse . BC.dropWhile (<= ' ') . B.reverse -- IMPROVE ?

readStr :: Lens' QuakeState SizeBufT -> Int -> BB.Builder -> Quake B.ByteString
readStr sizeBufLens idx acc
    | idx >= 2047 = return (BL.toStrict (BB.toLazyByteString acc))
    | otherwise = do
        c <- readChar sizeBufLens
        processChar c
  where
    processChar c
        | c `elem` [-1, 0, 0x0A] =
            return (BL.toStrict (BB.toLazyByteString acc))
        | otherwise = readStr sizeBufLens (idx + 1) (acc `mappend` BB.int8 c)

readString :: Lens' QuakeState SizeBufT -> Quake B.ByteString
readString sizeBufLens = buildString 0 mempty
  where
    buildString len acc
        | len >= 2047 = return (BL.toStrict (BB.toLazyByteString acc))
        | otherwise = do
            c <- readByte sizeBufLens
            processChar len acc c
    processChar len acc c
        | c `elem` [-1, 0] =
            return (BL.toStrict (BB.toLazyByteString acc))
        | otherwise = buildString (len + 1) (acc `mappend` BB.word8 (fromIntegral c))

readChar :: Lens' QuakeState SizeBufT -> Quake Int8
readChar sizeBufLens = doReadChar sizeBufLens =<< use sizeBufLens

doReadChar :: Lens' QuakeState SizeBufT -> SizeBufT -> Quake Int8
doReadChar sizeBufLens msgRead = do
    sizeBufLens.sbReadCount += 1
    return c
  where
    c | (msgRead^.sbReadCount) + 1 > (msgRead^.sbCurSize) = -1
      | otherwise = fromIntegral (B.index (msgRead^.sbData) (msgRead^.sbReadCount))

readPos :: Lens' QuakeState SizeBufT -> Quake (V3 Float)
readPos sizeBufLens = V3 <$> readCoord sizeBufLens
                         <*> readCoord sizeBufLens
                         <*> readCoord sizeBufLens

readCoord :: Lens' QuakeState SizeBufT -> Quake Float
readCoord sizeBufLens = fmap ((* (1.0 / 8.0)) . fromIntegral) (readShort sizeBufLens)

readAngle :: Lens' QuakeState SizeBufT -> Quake Float
readAngle sizeBufLens = fmap ((* (360.0 / 256)) . fromIntegral) (readChar sizeBufLens)

readAngle16 :: Lens' QuakeState SizeBufT -> Quake Float
readAngle16 sizeBufLens = fmap Math3D.shortToAngle (readShort sizeBufLens)

writeDeltaUserCmd :: Traversal' QuakeState SizeBufT -> UserCmdT -> UserCmdT -> Quake ()
writeDeltaUserCmd sizeBufLens from cmd = do
    writeByteI sizeBufLens (fromIntegral bits)
    when (bits .&. Constants.cmAngle1 /= 0) $
      writeShort sizeBufLens (fromIntegral (cmd^.ucAngles._x))
    when (bits .&. Constants.cmAngle2 /= 0) $
      writeShort sizeBufLens (fromIntegral (cmd^.ucAngles._y))
    when (bits .&. Constants.cmAngle3 /= 0) $
      writeShort sizeBufLens (fromIntegral (cmd^.ucAngles._z))
    when (bits .&. Constants.cmForward /= 0) $
      writeShort sizeBufLens (fromIntegral (cmd^.ucForwardMove))
    when (bits .&. Constants.cmSide /= 0) $
      writeShort sizeBufLens (fromIntegral (cmd^.ucSideMove))
    when (bits .&. Constants.cmUp /= 0) $
      writeShort sizeBufLens (fromIntegral (cmd^.ucUpMove))
    when (bits .&. Constants.cmButtons /= 0) $
      writeByteI sizeBufLens (fromIntegral (cmd^.ucButtons))
    when (bits .&. Constants.cmImpulse /= 0) $
      writeByteI sizeBufLens (fromIntegral (cmd^.ucImpulse))
    writeByteI sizeBufLens (fromIntegral (cmd^.ucMsec))
    writeByteI sizeBufLens (fromIntegral (cmd^.ucLightLevel))
  where
    a = fromBool Constants.cmAngle1 ((cmd^.ucAngles._x) /= (from^.ucAngles._x))
    b = fromBool Constants.cmAngle2 ((cmd^.ucAngles._y) /= (from^.ucAngles._y))
    c = fromBool Constants.cmAngle3 ((cmd^.ucAngles._z) /= (from^.ucAngles._z))
    d = fromBool Constants.cmForward ((cmd^.ucForwardMove) /= (from^.ucForwardMove))
    e = fromBool Constants.cmSide ((cmd^.ucSideMove) /= (from^.ucSideMove))
    f = fromBool Constants.cmUp ((cmd^.ucUpMove) /= (from^.ucUpMove))
    g = fromBool Constants.cmButtons ((cmd^.ucButtons) /= (from^.ucButtons))
    h = fromBool Constants.cmImpulse ((cmd^.ucImpulse) /= (from^.ucImpulse))
    bits = a .|. b .|. c .|. d .|. e .|. f .|. g .|. h
    fromBool v p = if p then v else 0

readData :: Lens' QuakeState SizeBufT -> Lens' QuakeState (UV.Vector Word8) -> Int -> Quake ()
readData sizeBufLens bufLens len = do
    updates <- collectUpdates 0 []
    bufLens %= (UV.// updates)
  where 
    collectUpdates idx acc
        | idx >= len = return acc
        | otherwise = do
            w <- readByte sizeBufLens
            collectUpdates (idx + 1) ((idx, fromIntegral w) : acc)

readDeltaUserCmd :: Lens' QuakeState SizeBufT -> UserCmdT -> Quake UserCmdT
readDeltaUserCmd sizeBufLens from = do
    bits <- fmap fromIntegral (readByte sizeBufLens)
    -- read current angles
    a1 <- if bits .&. Constants.cmAngle1 /= 0 then fmap fromIntegral (readShort sizeBufLens) else return (from^.ucAngles._x)
    a2 <- if bits .&. Constants.cmAngle2 /= 0 then fmap fromIntegral (readShort sizeBufLens) else return (from^.ucAngles._y)
    a3 <- if bits .&. Constants.cmAngle3 /= 0 then fmap fromIntegral (readShort sizeBufLens) else return (from^.ucAngles._z)
    -- read movement
    forward <- if bits .&. Constants.cmForward /= 0 then fmap fromIntegral (readShort sizeBufLens) else return (from^.ucForwardMove)
    side <- if bits .&. Constants.cmSide /= 0 then fmap fromIntegral (readShort sizeBufLens) else return (from^.ucSideMove)
    up <- if bits .&. Constants.cmUp /= 0 then fmap fromIntegral (readShort sizeBufLens) else return (from^.ucUpMove)
    -- read buttons
    buttons <- if bits .&. Constants.cmButtons /= 0 then fmap fromIntegral (readByte sizeBufLens) else return (from^.ucButtons)
    impulse <- if bits .&. Constants.cmImpulse /= 0 then fmap fromIntegral (readByte sizeBufLens) else return (from^.ucImpulse)
    -- read time to run command
    msec <- readByte sizeBufLens
    -- read the light level
    lightLevel <- readByte sizeBufLens
    return from { _ucAngles      = V3 a1 a2 a3
                , _ucForwardMove = forward
                , _ucSideMove    = side
                , _ucUpMove      = up
                , _ucButtons     = buttons
                , _ucImpulse     = impulse
                , _ucMsec        = fromIntegral msec
                , _ucLightLevel  = fromIntegral lightLevel
                }

readDir :: Lens' QuakeState SizeBufT -> Quake (V3 Float)
readDir sizeBufLens = do
    b <- readByte sizeBufLens
    when (b >= Constants.numVertexNormals) $
        Com.comError Constants.errDrop "MSG_ReadDir: out of range"
    return (Constants.byteDirs V.! b)

writeDeltaEntity :: EntityStateT -> EntityStateT -> Traversal' QuakeState SizeBufT -> Bool -> Bool -> Quake ()
writeDeltaEntity from to sizeBufLens force newEntity = do
    when ((to^.esNumber) == 0) $
        Com.fatalError "Unset entity number"
    when ((to^.esNumber) >= Constants.maxEdicts) $
        Com.fatalError "Entity number >= MAX_EDICTS"
    let a = if (to^.esNumber) >= 256 then Constants.uNumber16 else 0 -- number8 is implicit otherwise
        b = if (to^.esOrigin._x) /= (from^.esOrigin._x) then Constants.uOrigin1 else 0
        c = if (to^.esOrigin._y) /= (from^.esOrigin._y) then Constants.uOrigin2 else 0
        d = if (to^.esOrigin._z) /= (from^.esOrigin._z) then Constants.uOrigin3 else 0
        e = if (to^.esAngles._x) /= (from^.esAngles._x) then Constants.uAngle1 else 0
        f = if (to^.esAngles._y) /= (from^.esAngles._y) then Constants.uAngle2 else 0
        g = if (to^.esAngles._z) /= (from^.esAngles._z) then Constants.uAngle3 else 0
        h = if (to^.esSkinNum) /= (from^.esSkinNum) then skinNumValue else 0
        i = if (to^.esFrame) /= (from^.esFrame) then frameValue else 0
        j = if (to^.esEffects) /= (from^.esEffects) then effectsValue else 0
        k = if (to^.esRenderFx) /= (from^.esRenderFx) then renderFxValue else 0
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
    unless (bits == 0 && not force) $ do
        let finalBits | bits .&. 0xFF000000 /= 0 = bits .|. Constants.uMoreBits3 .|. Constants.uMoreBits2 .|. Constants.uMoreBits1
                      | bits .&. 0x00FF0000 /= 0 = bits .|. Constants.uMoreBits2 .|. Constants.uMoreBits1
                      | bits .&. 0x0000FF00 /= 0 = bits .|. Constants.uMoreBits1
                      | otherwise                = bits
        writeByteI sizeBufLens (finalBits .&. 255)
        writeFinaBits finalBits
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
        writeSkin finalBits
        writeEffects finalBits
        writeRenderFx finalBits
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
  where
    skinNumValue
        | (to^.esSkinNum) < 256     = Constants.uSkin8
        | (to^.esSkinNum) < 0x10000 = Constants.uSkin16
        | otherwise                 = Constants.uSkin8 .|. Constants.uSkin16
    frameValue
        | (to^.esFrame) < 256 = Constants.uFrame8
        | otherwise           = Constants.uFrame16
    effectsValue
        | (to^.esEffects) < 256    = Constants.uEffects8
        | (to^.esEffects) < 0x8000 = Constants.uEffects16
        | otherwise                = Constants.uEffects8 .|. Constants.uEffects16
    renderFxValue
        | (to^.esRenderFx) < 256    = Constants.uRenderFx8
        | (to^.esRenderFx) < 0x8000 = Constants.uRenderFx16
        | otherwise                 = Constants.uRenderFx8 .|. Constants.uRenderFx16
    writeFinaBits finalBits
        | finalBits .&. 0xFF000000 /= 0 = do
            writeByteI sizeBufLens ((finalBits `shiftR`  8) .&. 0xFF)
            writeByteI sizeBufLens ((finalBits `shiftR` 16) .&. 0xFF)
            writeByteI sizeBufLens ((finalBits `shiftR` 24) .&. 0xFF)
        | finalBits .&. 0x00FF0000 /= 0 = do
            writeByteI sizeBufLens ((finalBits `shiftR`  8) .&. 0xFF)
            writeByteI sizeBufLens ((finalBits `shiftR` 16) .&. 0xFF)
        | finalBits .&. 0x0000FF00 /= 0 = do
            writeByteI sizeBufLens ((finalBits `shiftR` 8) .&. 0xFF)
        | otherwise = return ()
    writeSkin finalBits
        | finalBits .&. Constants.uSkin8 /= 0 && finalBits .&. Constants.uSkin16 /= 0 = -- used for laser colors
            writeInt sizeBufLens (to^.esSkinNum)
        | finalBits .&. Constants.uSkin8 /= 0 =
            writeByteI sizeBufLens (to^.esSkinNum)
        | finalBits .&. Constants.uSkin16 /= 0 =
            writeShort sizeBufLens (to^.esSkinNum)
        | otherwise = return ()
    writeEffects finalBits
        | finalBits .&. (Constants.uEffects8 .|. Constants.uEffects16) == (Constants.uEffects8 .|. Constants.uEffects16) =
            writeInt sizeBufLens (to^.esEffects)
        | finalBits .&. Constants.uEffects8 /= 0 =
            writeByteI sizeBufLens (to^.esEffects)
        | finalBits .&. Constants.uEffects16 /= 0 =
            writeShort sizeBufLens (to^.esEffects)
        | otherwise = return ()
    writeRenderFx finalBits
        | finalBits .&. (Constants.uRenderFx8 .|. Constants.uRenderFx16) == (Constants.uRenderFx8 .|. Constants.uRenderFx16) =
            writeInt sizeBufLens (to^.esRenderFx)
        | finalBits .&. Constants.uRenderFx8 /= 0 =
            writeByteI sizeBufLens (to^.esRenderFx)
        | finalBits .&. Constants.uRenderFx16 /= 0 =
            writeShort sizeBufLens (to^.esRenderFx)
        | otherwise = return ()
