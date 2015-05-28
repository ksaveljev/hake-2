{-# LANGUAGE Rank2Types #-}
module Client.CLEnts where

import Control.Lens (use, Traversal')
import Data.Bits (shiftL, (.&.), (.|.))

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

    return (number, total''' : tail bits)

parseDelta :: EntityStateT -> Traversal' QuakeState EntityStateT -> Int -> Int -> Quake ()
parseDelta _ _ _ _ = do
    io (putStrLn "CLEnts.parseDelta") >> undefined -- TODO
