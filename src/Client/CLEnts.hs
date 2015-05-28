{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiWayIf #-}
module Client.CLEnts where

import Control.Lens (use, (^.), (.=), Traversal')
import Data.Bits (shiftL, (.&.), (.|.))
import Linear (V3(..), _x, _y, _z)

import Quake
import QuakeState
import qualified Constants
import qualified QCommon.MSG as MSG

addEntities :: Quake ()
addEntities = do
    io (putStrLn "CLEnts.addEntities") >> undefined -- TODO

{-
- ================= CL_ParseEntityBits
- 
- Returns the entity number and the header bits =================
-}
parseEntityBits :: [Int] -> Quake (Int, [Int])
parseEntityBits bits = do
    total <- MSG.readByte (globals.netMessage)

    total' <- if total .&. Constants.uMoreBits1 /= 0
                then do
                  b <- MSG.readByte (globals.netMessage)
                  return (total .|. (b `shiftL` 8))
                else
                  return total

    total'' <- if total' .&. Constants.uMoreBits2 /= 0
                 then do
                   b <- MSG.readByte (globals.netMessage)
                   return (total' .|. (b `shiftL` 16))
                 else
                   return total'

    total''' <- if total'' .&. Constants.uMoreBits3 /= 0
                  then do
                    b <- MSG.readByte (globals.netMessage)
                    return (total'' .|. (b `shiftL` 24))
                  else
                    return total''

    number <- if total''' .&. Constants.uNumber16 /= 0
                then MSG.readShort (globals.netMessage)
                else MSG.readByte (globals.netMessage)

    io (print "YEYEYE")
    io (print total''')
    io (print number)

    return (number, total''' : tail bits)

{-
- ================== CL_ParseDelta
- 
- Can go from either a baseline or a previous packet_entity
- ==================
-}
parseDelta :: EntityStateT -> Traversal' QuakeState EntityStateT -> Int -> Int -> Quake ()
parseDelta from to number bits = do
    modelIndex <- if bits .&. Constants.uModel /= 0
                    then MSG.readByte (globals.netMessage)
                    else return (from^.esModelIndex)

    modelIndex2 <- if bits .&. Constants.uModel2 /= 0
                     then MSG.readByte (globals.netMessage)
                     else return (from^.esModelIndex2)

    modelIndex3 <- if bits .&. Constants.uModel3 /= 0
                     then MSG.readByte (globals.netMessage)
                     else return (from^.esModelIndex3)

    modelIndex4 <- if bits .&. Constants.uModel4 /= 0
                     then MSG.readByte (globals.netMessage)
                     else return (from^.esModelIndex4)

    frame <- if bits .&. Constants.uFrame8 /= 0
               then MSG.readByte (globals.netMessage)
               else return (from^.esFrame)

    frame' <- if bits .&. Constants.uFrame16 /= 0
                then MSG.readShort (globals.netMessage)
                else return frame

    skinNum <- if | bits .&. Constants.uSkin8 /= 0 && bits .&. Constants.uSkin16 /= 0 ->
                      MSG.readLong (globals.netMessage)
                  | bits .&. Constants.uSkin8 /= 0 ->
                      MSG.readByte (globals.netMessage)
                  | bits .&. Constants.uSkin16 /= 0 ->
                      MSG.readShort (globals.netMessage)
                  | otherwise -> return (from^.esSkinNum)

    effects <- if | bits .&. (Constants.uEffects8 .|. Constants.uEffects16) == (Constants.uEffects8 .|. Constants.uEffects16) ->
                      MSG.readLong (globals.netMessage)
                  | bits .&. Constants.uEffects8 /= 0 ->
                      MSG.readByte (globals.netMessage)
                  | bits .&. Constants.uEffects16 /= 0 ->
                      MSG.readShort (globals.netMessage)
                  | otherwise ->
                      return (from^.esEffects)

    renderFx <- if | bits .&. (Constants.uRenderFx8 .|. Constants.uRenderFx16) == (Constants.uRenderFx8 .|. Constants.uRenderFx16) ->
                       MSG.readLong (globals.netMessage)
                   | bits .&. Constants.uRenderFx8 /= 0 ->
                       MSG.readByte (globals.netMessage)
                   | bits .&. Constants.uRenderFx16 /= 0 ->
                       MSG.readShort (globals.netMessage)
                   | otherwise -> return (from^.esRenderFx)

    originX <- if bits .&. Constants.uOrigin1 /= 0
                 then MSG.readCoord (globals.netMessage)
                 else return (from^.esOrigin._x)

    originY <- if bits .&. Constants.uOrigin2 /= 0
                 then MSG.readCoord (globals.netMessage)
                 else return (from^.esOrigin._y)

    originZ <- if bits .&. Constants.uOrigin3 /= 0
                 then MSG.readCoord (globals.netMessage)
                 else return (from^.esOrigin._z)

    anglesX <- if bits .&. Constants.uAngle1 /= 0
                 then MSG.readAngle (globals.netMessage)
                 else return (from^.esAngles._x)

    anglesY <- if bits .&. Constants.uAngle2 /= 0
                 then MSG.readAngle (globals.netMessage)
                 else return (from^.esAngles._y)

    anglesZ <- if bits .&. Constants.uAngle3 /= 0
                 then MSG.readAngle (globals.netMessage)
                 else return (from^.esAngles._z)

    oldOrigin <- if bits .&. Constants.uOldOrigin /= 0
                   then MSG.readPos (globals.netMessage)
                   else return (from^.esOrigin)

    sound <- if bits .&. Constants.uSound /= 0
               then MSG.readByte (globals.netMessage)
               else return (from^.esSound)

    event <- if bits .&. Constants.uEvent /= 0
               then MSG.readByte (globals.netMessage)
               else return 0

    solid <- if bits .&. Constants.uSolid /= 0
               then MSG.readShort (globals.netMessage)
               else return (from^.esSolid)

    to .= from { _esNumber      = number
               , _esModelIndex  = modelIndex
               , _esModelIndex2 = modelIndex2
               , _esModelIndex3 = modelIndex3
               , _esModelIndex4 = modelIndex4
               , _esFrame       = frame'
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

    io (print "ENTITYENTITY")
    io (print number)
    io (print modelIndex)
    io (print modelIndex2)
    io (print modelIndex3)
    io (print modelIndex4)
    io (print frame')
    io (print skinNum)
    io (print effects)
    io (print renderFx)
    io (print originX)
    io (print originY)
    io (print originZ)
    io (print anglesX)
    io (print anglesY)
    io (print anglesZ)
    io (print oldOrigin)
    io (print sound)
    io (print event)
    io (print solid)
