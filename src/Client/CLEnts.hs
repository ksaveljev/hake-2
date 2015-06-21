{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiWayIf #-}
module Client.CLEnts where

import Control.Lens (use, (^.), (.=), Traversal', preuse, ix, Lens')
import Control.Monad (when, liftM, unless)
import Data.Bits (shiftL, (.&.), (.|.))
import Data.Int (Int16)
import Linear (V3(..), V4(..), _x, _y, _z, _w)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import Quake
import QuakeState
import CVarVariables
import qualified Constants
import qualified Client.CLFX as CLFX
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
parsePlayerState oldFrame newFrameLens = do
    let state = case oldFrame of
                  Nothing -> newPlayerStateT
                  Just frame -> frame^.fPlayerState

    flags <- MSG.readShort (globals.netMessage)

    -- parse the pmove_state_t
    pmType <- if flags .&. Constants.psMType /= 0
                then MSG.readByte (globals.netMessage)
                else return (state^.psPMoveState.pmsPMType)

    origin <- if flags .&. Constants.psMOrigin /= 0
                then do
                  x <- MSG.readShort (globals.netMessage)
                  y <- MSG.readShort (globals.netMessage)
                  z <- MSG.readShort (globals.netMessage)
                  return $ fmap fromIntegral (V3 x y z)
                else
                  return (state^.psPMoveState.pmsOrigin)

    velocity <- if flags .&. Constants.psMVelocity /= 0
                  then do
                    x <- MSG.readShort (globals.netMessage)
                    y <- MSG.readShort (globals.netMessage)
                    z <- MSG.readShort (globals.netMessage)
                    return $ fmap fromIntegral (V3 x y z)
                  else
                    return (state^.psPMoveState.pmsVelocity)

    pmTime <- if flags .&. Constants.psMTime /= 0
                then liftM fromIntegral $ MSG.readByte (globals.netMessage)
                else return (state^.psPMoveState.pmsPMTime)

    pmFlags <- if flags .&. Constants.psMFlags /= 0
                 then liftM fromIntegral $ MSG.readByte (globals.netMessage)
                 else return (state^.psPMoveState.pmsPMFlags)

    gravity <- if flags .&. Constants.psMGravity /= 0
                 then liftM fromIntegral $ MSG.readShort (globals.netMessage)
                 else return (state^.psPMoveState.pmsGravity)

    deltaAngles <- if flags .&. Constants.psMDeltaAngles /= 0
                     then do
                       x <- MSG.readShort (globals.netMessage)
                       y <- MSG.readShort (globals.netMessage)
                       z <- MSG.readShort (globals.netMessage)
                       return $ fmap fromIntegral (V3 x y z)
                     else
                       return (state^.psPMoveState.pmsDeltaAngles)

    attractLoop <- use $ globals.cl.csAttractLoop
    let pmType' = if attractLoop
                    then Constants.pmFreeze -- demo playback
                    else pmType

    -- parse the rest of the player_state_t
    viewOffset <- if flags .&. Constants.psViewOffset /= 0
                    then do
                      x <- MSG.readChar (globals.netMessage)
                      y <- MSG.readChar (globals.netMessage)
                      z <- MSG.readChar (globals.netMessage)
                      return $ fmap ((* 0.25) . fromIntegral) (V3 x y z)
                    else
                      return (state^.psViewOffset)

    viewAngles <- if flags .&. Constants.psViewAngles /= 0
                    then do
                      x <- MSG.readAngle16 (globals.netMessage)
                      y <- MSG.readAngle16 (globals.netMessage)
                      z <- MSG.readAngle16 (globals.netMessage)
                      return (V3 x y z)
                    else
                      return (state^.psViewAngles)

    kickAngles <- if flags .&. Constants.psKickAngles /= 0
                    then do
                      x <- MSG.readChar (globals.netMessage)
                      y <- MSG.readChar (globals.netMessage)
                      z <- MSG.readChar (globals.netMessage)
                      return $ fmap ((* 0.25) . fromIntegral) (V3 x y z)
                    else
                      return (state^.psKickAngles)

    gunIndex <- if flags .&. Constants.psWeaponIndex /= 0
                  then MSG.readByte (globals.netMessage)
                  else return (state^.psGunIndex)

    (gunFrame, gunOffset, gunAngles) <- if flags .&. Constants.psWeaponFrame /= 0
                                          then do
                                            gunFrame <- MSG.readByte (globals.netMessage)

                                            x <- MSG.readChar (globals.netMessage)
                                            y <- MSG.readChar (globals.netMessage)
                                            z <- MSG.readChar (globals.netMessage)

                                            x' <- MSG.readChar (globals.netMessage)
                                            y' <- MSG.readChar (globals.netMessage)
                                            z' <- MSG.readChar (globals.netMessage)

                                            return (gunFrame, fmap ((* 0.25) . fromIntegral) (V3 x y z), fmap ((* 0.25) . fromIntegral) (V3 x' y' z'))
                                          else
                                            return (state^.psGunFrame, state^.psGunOffset, state^.psGunAngles)

    blend <- if flags .&. Constants.psBlend /= 0
               then do
                 x <- MSG.readByte (globals.netMessage)
                 y <- MSG.readByte (globals.netMessage)
                 z <- MSG.readByte (globals.netMessage)
                 w <- MSG.readByte (globals.netMessage)
                 return $ fmap ((/ 255) . fromIntegral) (V4 x y z w)
               else
                 return (state^.psBlend)

    fov <- if flags .&. Constants.psFov /= 0
             then liftM fromIntegral $ MSG.readByte (globals.netMessage)
             else return (state^.psFOV)

    rdFlags <- if flags .&. Constants.psRdFlags /= 0
                 then MSG.readByte (globals.netMessage)
                 else return (state^.psRDFlags)

    -- parse stats
    statbits <- MSG.readLong (globals.netMessage)
    updates <- readStats statbits 0 Constants.maxStats []

    newFrameLens.fPlayerState .=
      PlayerStateT { _psPMoveState = PMoveStateT { _pmsPMType      = pmType
                                                 , _pmsOrigin      = origin
                                                 , _pmsVelocity    = velocity
                                                 , _pmsPMFlags     = pmFlags
                                                 , _pmsPMTime      = pmTime
                                                 , _pmsGravity     = gravity
                                                 , _pmsDeltaAngles = deltaAngles
                                                 }
                   , _psViewAngles = viewAngles
                   , _psViewOffset = viewOffset
                   , _psKickAngles = kickAngles
                   , _psGunAngles  = gunAngles
                   , _psGunOffset  = gunOffset
                   , _psGunIndex   = gunIndex
                   , _psGunFrame   = gunFrame
                   , _psBlend      = blend
                   , _psFOV        = fov
                   , _psRDFlags    = rdFlags
                   , _psStats      = (state^.psStats) UV.// updates
                   }

  where readStats :: Int -> Int -> Int -> [(Int, Int16)] -> Quake [(Int, Int16)]
        readStats statbits idx maxIdx acc
          | idx >= maxIdx = return acc
          | otherwise = do
              if statbits .&. (1 `shiftL` idx) /= 0
                then do
                  v <- MSG.readShort (globals.netMessage)
                  readStats statbits (idx + 1) maxIdx ((idx, fromIntegral v) : acc)
                else
                  readStats statbits (idx + 1) maxIdx acc

parsePacketEntities :: Maybe FrameT -> Lens' QuakeState FrameT -> Quake ()
parsePacketEntities _ _ = do
    io (putStrLn "CLEnts.parsePacketEntities") >> undefined -- TODO

fireEntityEvents :: FrameT -> Quake ()
fireEntityEvents frame = do
    parseEntities <- use $ globals.clParseEntities
    goThrouhEntities parseEntities 0 (frame^.fNumEntities)

  where goThrouhEntities :: V.Vector EntityStateT -> Int -> Int -> Quake ()
        goThrouhEntities parseEntities pnum maxPnum
          | pnum >= maxPnum = return ()
          | otherwise = do
              let num = ((frame^.fParseEntities) + pnum) .&. (Constants.maxParseEntities - 1)
                  s1 = parseEntities V.! num

              when ((s1^.esEvent) /= 0) $
                CLFX.entityEvent s1

              -- EF_TELEPORTER acts like an event, but is not cleared each frame
              when ((s1^.esEffects) .&. Constants.efTeleporter /= 0) $
                CLFX.teleporterParticles s1

              goThrouhEntities parseEntities (pnum + 1) maxPnum
