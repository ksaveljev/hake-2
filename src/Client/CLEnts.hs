{-# LANGUAGE Rank2Types #-}
module Client.CLEnts
    ( addEntities
    , parseDelta
    , parseEntityBits
    , parseFrame
    ) where

import           Control.Lens      (Traversal', (.=), (^.))
import           Data.Bits         (shiftL, (.&.), (.|.))
import           Linear            (V3(..), _x, _y, _z)

import qualified Constants
import           Game.EntityStateT
import qualified QCommon.MSG       as MSG
import           QuakeState
import           Types

addEntities :: Quake ()
addEntities = error "CLEnts.addEntities" -- TODO

parseFrame :: Quake ()
parseFrame = error "CLEnts.parseFrame" -- TODO

parseEntityBits :: [Int] -> Quake (Int, [Int])
parseEntityBits bits = readA >>= readB >>= readC >>= readD >>= readNumber
  where
    readA = MSG.readByte (globals.gNetMessage)
    readB a | a .&. Constants.uMoreBits1 /= 0 = do
                b <- MSG.readByte (globals.gNetMessage)
                return (a .|. (b `shiftL` 8))
            | otherwise = return a
    readC b | b .&. Constants.uMoreBits2 /= 0 = do
                c <- MSG.readByte (globals.gNetMessage)
                return (b .|. (c `shiftL` 16))
            | otherwise = return b
    readD c | c .&. Constants.uMoreBits3 /= 0 = do
                d <- MSG.readByte (globals.gNetMessage)
                return (c .|. (d `shiftL` 24))
            | otherwise = return c
    readNumber d | d .&. Constants.uNumber16 /= 0 = do
                     num <- MSG.readShort (globals.gNetMessage)
                     return (num, d : tail bits)
                 | otherwise = do
                     num <- MSG.readByte (globals.gNetMessage)
                     return (num, d : tail bits)

parseDelta :: EntityStateT -> Traversal' QuakeState EntityStateT -> Int -> Int -> Quake ()
parseDelta from to number bits = do
    modelIndex <- getModelIndex
    modelIndex2 <- getModelIndex2
    modelIndex3 <- getModelIndex3
    modelIndex4 <- getModelIndex4
    frame <- getFrame
    skinNum <- getSkinNum
    effects <- getEffects
    renderFx <- getRenderFx
    originX <- getOriginX
    originY <- getOriginY
    originZ <- getOriginZ
    anglesX <- getAnglesX
    anglesY <- getAnglesY
    anglesZ <- getAnglesZ
    oldOrigin <- getOldOrigin
    sound <- getSound
    event <- getEvent
    solid <- getSolid
    to .= from { _esNumber      = number
               , _esModelIndex  = modelIndex
               , _esModelIndex2 = modelIndex2
               , _esModelIndex3 = modelIndex3
               , _esModelIndex4 = modelIndex4
               , _esFrame       = frame
               , _esSkinNum     = skinNum
               , _esEffects     = effects
               , _esRenderFx    = renderFx
               , _esOrigin      = V3 originX originY originZ
               , _esAngles      = V3 anglesX anglesY anglesZ
               , _esOldOrigin   = oldOrigin
               , _esSound       = sound
               , _esEvent       = event
               , _esSolid       = solid
               }
  where
    getModelIndex
        | bits .&. Constants.uModel /= 0 = MSG.readByte (globals.gNetMessage)
        | otherwise = return (from^.esModelIndex)
    getModelIndex2
        | bits .&. Constants.uModel2 /= 0 = MSG.readByte (globals.gNetMessage)
        | otherwise = return (from^.esModelIndex2)
    getModelIndex3
        | bits .&. Constants.uModel3 /= 0 = MSG.readByte (globals.gNetMessage)
        | otherwise = return (from^.esModelIndex3)
    getModelIndex4
        | bits .&. Constants.uModel4 /= 0 = MSG.readByte (globals.gNetMessage)
        | otherwise = return (from^.esModelIndex4)
    getFrame
        | bits .&. Constants.uFrame8 /= 0 = MSG.readByte (globals.gNetMessage)
        | bits .&. Constants.uFrame16 /= 0 = MSG.readShort (globals.gNetMessage)
        | otherwise = return (from^.esFrame)
    getSkinNum
        | bits .&. Constants.uSkin8 /= 0 && bits .&. Constants.uSkin16 /= 0 = MSG.readLong (globals.gNetMessage)
        | bits .&. Constants.uSkin8 /= 0 = MSG.readByte (globals.gNetMessage)
        | bits .&. Constants.uSkin16 /= 0 = MSG.readShort (globals.gNetMessage)
        | otherwise = return (from^.esSkinNum)
    getEffects
        | bits .&. (Constants.uEffects8 .|. Constants.uEffects16) == (Constants.uEffects8 .|. Constants.uEffects16) = MSG.readLong (globals.gNetMessage)
        | bits .&. Constants.uEffects8 /= 0 = MSG.readByte (globals.gNetMessage)
        | bits .&. Constants.uEffects16 /= 0 = MSG.readShort (globals.gNetMessage)
        | otherwise = return (from^.esEffects)
    getRenderFx
        | bits .&. (Constants.uRenderFx8 .|. Constants.uRenderFx16) == (Constants.uRenderFx8 .|. Constants.uRenderFx16) = MSG.readLong (globals.gNetMessage)
        | bits .&. Constants.uRenderFx8 /= 0 = MSG.readByte (globals.gNetMessage)
        | bits .&. Constants.uRenderFx16 /= 0 = MSG.readShort (globals.gNetMessage)
        | otherwise = return (from^.esRenderFx)
    getOriginX
        | bits .&. Constants.uOrigin1 /= 0 = MSG.readCoord (globals.gNetMessage)
        | otherwise = return (from^.esOrigin._x)
    getOriginY
        | bits .&. Constants.uOrigin2 /= 0 = MSG.readCoord (globals.gNetMessage)
        | otherwise = return (from^.esOrigin._y)
    getOriginZ
        | bits .&. Constants.uOrigin3 /= 0 = MSG.readCoord (globals.gNetMessage)
        | otherwise = return (from^.esOrigin._z)
    getAnglesX
        | bits .&. Constants.uAngle1 /= 0 = MSG.readAngle (globals.gNetMessage)
        | otherwise = return (from^.esAngles._x)
    getAnglesY
        | bits .&. Constants.uAngle2 /= 0 = MSG.readAngle (globals.gNetMessage)
        | otherwise = return (from^.esAngles._y)
    getAnglesZ
        | bits .&. Constants.uAngle3 /= 0 = MSG.readAngle (globals.gNetMessage)
        | otherwise = return (from^.esAngles._z)
    getOldOrigin
        | bits .&. Constants.uOldOrigin /= 0 = MSG.readPos (globals.gNetMessage)
        | otherwise = return (from^.esOrigin)
    getSound
        | bits .&. Constants.uSound /= 0 = MSG.readByte (globals.gNetMessage)
        | otherwise = return (from^.esSound)
    getEvent
        | bits .&. Constants.uEvent /= 0 = MSG.readByte (globals.gNetMessage)
        | otherwise = return 0
    getSolid
        | bits .&. Constants.uSolid /= 0 = MSG.readShort (globals.gNetMessage)
        | otherwise = return (from^.esSolid)