{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiWayIf #-}
module Client.CLEnts where

import Control.Lens (use, (^.), (.=), Traversal', preuse, ix, Lens')
import Control.Monad (when, liftM, unless)
import Data.Bits (shiftL, (.&.), (.|.))
import Linear (V3(..), _x, _y, _z)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V

import Quake
import QuakeState
import CVarVariables
import qualified Constants
import {-# SOURCE #-} qualified Client.CLParse as CLParse
import qualified Client.CLPred as CLPred
import {-# SOURCE #-} qualified Client.SCR as SCR
import qualified QCommon.Com as Com
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

parseFrame :: Quake ()
parseFrame = do
    globals.cl.csFrame .= newFrameT

    serverFrame <- MSG.readLong (globals.netMessage)
    globals.cl.csFrame.fServerFrame .= serverFrame

    deltaFrame <- MSG.readLong (globals.netMessage)
    globals.cl.csFrame.fDeltaFrame .= deltaFrame

    let serverTime = serverFrame * 100
    globals.cl.csFrame.fServerTime .= serverTime

    -- BIG HACK to let old demos continue to work
    serverProtocol <- use $ globals.cls.csServerProtocol
    when (serverProtocol /= 26) $ do
      surpressCount <- MSG.readByte (globals.netMessage)
      globals.cl.csSurpressCount .= surpressCount

    showNetValue <- liftM (^.cvValue) clShowNetCVar
    when (showNetValue == 3) $
      Com.printf ("   frame:" `B.append` BC.pack (show serverFrame) `B.append` "  delta:" `B.append` BC.pack (show deltaFrame) `B.append` "\n") -- IMPROVE?

    -- If the frame is delta compressed from data that we
    -- no longer have available, we must suck up the rest of
    -- the frame, but not use it, then ask for a non-compressed
    -- message
    old <- if deltaFrame <= 0
             then do
               globals.cl.csFrame.fValid .= True -- uncompressed frame
               globals.cls.csDemoWaiting .= False -- we can start recording now
               return Nothing
             else do
               let idx = deltaFrame .&. Constants.updateMask
               Just old <- preuse $ globals.cl.csFrames.ix idx

               unless (old^.fValid) $ -- should never happen
                 Com.printf "Delta from invalid frame (not supposed to happen!).\n"

               parseEntities <- use $ globals.cl.csParseEntities

               if | (old^.fServerFrame) /= deltaFrame -> -- The frame is too old, so we can't reconstruct it properly.
                      Com.printf "Delta frame too old.\n"
                  | parseEntities - (old^.fParseEntities) > Constants.maxParseEntities - 128 ->
                      Com.printf "Delta parse_entities too old.\n"
                  | otherwise ->
                      globals.cl.csFrame.fValid .= True -- valid delta parse

               return (Just old)

    -- clamp time
    use (globals.cl.csTime) >>= \time ->
      if | time > serverTime ->
             globals.cl.csTime .= serverTime
         | time < serverTime - 100 ->
             globals.cl.csTime .= serverTime - 100
         | otherwise ->
             return ()


    -- read areabits
    len <- MSG.readByte (globals.netMessage)
    MSG.readData (globals.netMessage) (globals.cl.csFrame.fAreaBits) len

    -- read playerinfo
    cmd <- MSG.readByte (globals.netMessage)
    CLParse.showNet (CLParse.svcStrings V.! cmd)

    when (cmd /= Constants.svcPlayerInfo) $
      Com.comError Constants.errDrop "CL_ParseFrame: not playerinfo"

    parsePlayerState old (globals.cl.csFrame)

    -- read packet entities
    cmd' <- MSG.readByte (globals.netMessage)
    CLParse.showNet (CLParse.svcStrings V.! cmd')

    when (cmd' /= Constants.svcPacketEntities) $
      Com.comError Constants.errDrop "CL_ParseFrame: not packetentities"

    parsePacketEntities old (globals.cl.csFrame)

    -- save the frame off in the backup array for later delta comparisons
    frame <- use $ globals.cl.csFrame

    let idx = serverFrame .&. Constants.updateMask
    globals.cl.csFrames.ix idx .= frame

    when (frame^.fValid) $ do
      -- getting a valid frame message ends the connection process
      clientStatic <- use $ globals.cls
      clientState <- use $ globals.cl

      when ((clientStatic^.csState) /= Constants.caActive) $ do
        globals.cls.csState .= Constants.caActive
        globals.cl.csForceRefDef .= True

        globals.cl.csPredictedOrigin .= fmap ((* 0.125) . fromIntegral) (frame^.fPlayerState.psPMoveState.pmsOrigin)
        globals.cl.csPredictedAngles .= (frame^.fPlayerState.psViewAngles)

        when ((clientStatic^.csDisableServerCount) /= (clientState^.csServerCount) && (clientState^.csRefreshPrepped)) $
          SCR.endLoadingPlaque -- get rid of loading plaque

      globals.cl.csSoundPrepped .= True -- can start mixing ambient sounds

      -- fire entity events
      fireEntityEvents frame
      CLPred.checkPredictionError

parsePlayerState :: Maybe FrameT -> Lens' QuakeState FrameT -> Quake ()
parsePlayerState _ _ = do
    io (putStrLn "CLEnts.parsePlayerState") >> undefined -- TODO

parsePacketEntities :: Maybe FrameT -> Lens' QuakeState FrameT -> Quake ()
parsePacketEntities _ _ = do
    io (putStrLn "CLEnts.parsePacketEntities") >> undefined -- TODO

fireEntityEvents :: FrameT -> Quake ()
fireEntityEvents _ = do
    io (putStrLn "CLEnts.fireEntityEvents") >> undefined -- TODO
