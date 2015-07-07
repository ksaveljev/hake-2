{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiWayIf #-}
module Client.CLEnts where

import Control.Lens (use, (^.), (.=), Traversal', preuse, ix, Lens', (+=), (-=))
import Control.Monad (when, liftM, unless)
import Data.Bits (shiftL, shiftR, (.&.), (.|.), complement)
import Data.Int (Int16)
import Data.Maybe (fromJust, isNothing)
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
import qualified Client.CLTEnt as CLTEnt
import {-# SOURCE #-} qualified Client.SCR as SCR
import {-# SOURCE #-} qualified Client.V as ClientV
import qualified QCommon.Com as Com
import qualified QCommon.MSG as MSG
import qualified Util.Lib as Lib
import qualified Util.Math3D as Math3D

{-
- =============== CL_AddEntities
- 
- Emits all entities, particles, and lights to the refresh ===============
-}
addEntities :: Quake ()
addEntities = do
    cl' <- use $ globals.cl
    cls' <- use $ globals.cls

    showClampValue <- liftM (^.cvValue) clShowClampCVar

    when ((cls'^.csState) == Constants.caActive) $ do
      if | (cl'^.csTime) > (cl'^.csFrame.fServerTime) -> do
             when (showClampValue /= 0) $
               Com.printf ("high clamp " `B.append` BC.pack (show ((cl'^.csTime) - (cl'^.csFrame.fServerTime))) `B.append` "\n") -- IMPROVE?

             globals.cl.csTime .= (cl'^.csFrame.fServerTime)
             globals.cl.csLerpFrac .= 1

         | (cl'^.csTime) < (cl'^.csFrame.fServerTime) - 100 -> do
             when (showClampValue /= 0) $
               Com.printf ("low clamp " `B.append` BC.pack (show ((cl'^.csFrame.fServerTime) - 100 - (cl'^.csTime))) `B.append` "\n") -- IMPROVE?

             globals.cl.csTime .= (cl'^.csFrame.fServerTime) - 100
             globals.cl.csLerpFrac .= 0
             
         | otherwise -> do
             globals.cl.csLerpFrac .= 1 - fromIntegral ((cl'^.csFrame.fServerTime) - (cl'^.csTime)) * 0.01

      timeDemoValue <- liftM (^.cvValue) clTimeDemoCVar

      when (timeDemoValue /= 0) $ do
        globals.cl.csLerpFrac .= 1

      -- is ok.. CL_AddPacketEntities (cl.frame); CL_AddTEnts ();
      -- CL_AddParticles (); CL_AddDLights (); CL_AddLightStyles ();

      calcViewValues
      -- PMM - moved this here so the heat beam has the right values for
      -- the vieworg, and can lock the beam to the gun
      use (globals.cl.csFrame) >>= \f -> addPacketEntities f

      CLTEnt.addTEnts
      CLFX.addParticles
      CLFX.addDLights
      CLFX.addLightStyles

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

    io (print "SERVER FRAME")
    io (print serverFrame)
    io (print "DELTA FRAME")
    io (print deltaFrame)

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

{-
- ================== CL_ParsePacketEntities ==================
- 
- An svc_packetentities has just been parsed, deal with the rest of the
- data stream.
-}
parsePacketEntities :: Maybe FrameT -> Lens' QuakeState FrameT -> Quake ()
parsePacketEntities oldFrame newFrameLens = do
    use (globals.cl.csParseEntities) >>= \parseEntities -> do
      newFrameLens.fParseEntities .= parseEntities
      newFrameLens.fNumEntities .= 0

    -- delta from the entities present in oldframe
    parseEntities <- use $ globals.clParseEntities
    let (oldNum, oldState) = case oldFrame of
                               Nothing -> (99999, Nothing)
                               Just frame -> let idx = (frame^.fParseEntities) .&. (Constants.maxParseEntities - 1)
                                                 oldState = parseEntities V.! idx
                                             in (oldState^.esNumber, Just oldState)

    parse oldNum oldState 0 0

  where parse :: Int -> Maybe EntityStateT -> Int -> Int -> Quake ()
        parse oldNum oldState oldIndex bits = do
          (newNum, iw) <- parseEntityBits [bits]
          let bits' = head iw

          when (newNum >= Constants.maxEdicts) $
            Com.comError Constants.errDrop ("CL_ParsePacketEntities: bad number:" `B.append` (BC.pack $ show newNum)) -- IMPROVE?

          use (globals.netMessage) >>= \netMsg ->
            when ((netMsg^.sbReadCount) > (netMsg^.sbCurSize)) $
              Com.comError Constants.errDrop "CL_ParsePacketEntities: end of message"

          showNetValue <- liftM (^.cvValue) clShowNetCVar

          io (print $ "BITS = " ++ show bits')
          io (print $ "NEW NUM = " ++ show newNum)

          if newNum == 0
            then
              -- any remaining entities in the old frame are copied over
              copyRemainingEntities showNetValue oldNum oldState oldIndex
            else do
              -- one or more entities from the old packet are unchanged
              (oldIndex', oldNum', oldState') <- deltaEntityPackets showNetValue oldNum newNum oldState oldIndex

              (oldIndex'', oldNum'', oldState'') <- 
                if | bits' .&. Constants.uRemove /= 0 -> do -- the entity present in oldframe is not in the current frame
                       when (showNetValue == 3) $
                         Com.printf ("   remove: " `B.append` BC.pack (show newNum) `B.append` "\n") -- IMPROVE ?

                       when (oldNum' /= newNum) $
                         Com.printf "U_REMOVE: oldnum != newnum\n"

                       let Just oldFrame' = oldFrame
                       if (oldIndex' + 1) >= oldFrame'^.fNumEntities
                         then
                           return (oldIndex' + 1, 99999, oldState')
                         else do
                           let idx = ((oldFrame'^.fParseEntities) + oldIndex' + 1) .&. (Constants.maxParseEntities - 1)
                           Just oldState'' <- preuse $ globals.clParseEntities.ix idx
                           return (oldIndex' + 1, oldState''^.esNumber, Just oldState'')

                   | oldNum' == newNum -> do -- delta from previous state
                       when (showNetValue == 3) $
                         Com.printf ("   delta: " `B.append` BC.pack (show newNum) `B.append` "\n") -- IMPROVE ?

                       deltaEntity newFrameLens newNum (fromJust oldState) bits'

                       let Just oldFrame' = oldFrame
                       if (oldIndex' + 1) >= oldFrame'^.fNumEntities
                         then
                           return (oldIndex' + 1, 99999, oldState')
                         else do
                           let idx = ((oldFrame'^.fParseEntities) + oldIndex' + 1) .&. (Constants.maxParseEntities - 1)
                           Just oldState'' <- preuse $ globals.clParseEntities.ix idx
                           return (oldIndex' + 1, oldState''^.esNumber, Just oldState'')

                   | oldNum' > newNum -> do -- delta from baseline
                       when (showNetValue == 3) $
                         Com.printf ("   baseline: " `B.append` BC.pack (show newNum) `B.append` "\n") -- IMPROVE ?

                       Just baseline <- preuse $ globals.clEntities.ix newNum.ceBaseline
                       deltaEntity newFrameLens newNum baseline bits'

                       return (oldIndex', oldNum', oldState')

              parse oldNum'' oldState'' oldIndex'' bits'

        deltaEntityPackets :: Float -> Int -> Int -> Maybe EntityStateT -> Int -> Quake (Int, Int, Maybe EntityStateT)
        deltaEntityPackets showNetValue oldNum newNum oldState oldIndex
          | oldNum >= newNum = return (oldIndex, oldNum, oldState)
          | otherwise = do
              when (showNetValue == 3) $
                Com.printf ("   unchanged: " `B.append` BC.pack (show oldNum) `B.append` "\n") -- IMPROVE ?

              let Just oldFrame' = oldFrame
              deltaEntity newFrameLens oldNum (fromJust oldState) 0

              if (oldIndex + 1) >= (oldFrame'^.fNumEntities)
                then
                  deltaEntityPackets showNetValue 99999 newNum oldState (oldIndex + 1)
                else do
                  let idx = ((oldFrame'^.fParseEntities) + (oldIndex + 1)) .&. (Constants.maxParseEntities - 1)
                  Just oldState' <- preuse $ globals.clParseEntities.ix idx
                  deltaEntityPackets showNetValue (oldState'^.esNumber) newNum (Just oldState') (oldIndex + 1)

        copyRemainingEntities :: Float -> Int -> Maybe EntityStateT -> Int -> Quake ()
        copyRemainingEntities showNetValue oldNum oldState oldIndex
          | oldNum == 99999 = return ()
          | otherwise = do -- one or more entities from the old packet are unchanged
              when (showNetValue == 3) $
                Com.printf ("   unchanged: " `B.append` BC.pack (show oldNum) `B.append` "\n") -- IMPROVE?

              deltaEntity newFrameLens oldNum (fromJust oldState) 0

              let Just oldFrame' = oldFrame

              if oldIndex + 1 >= oldFrame'^.fNumEntities
                then
                  copyRemainingEntities showNetValue 99999 oldState (oldIndex + 1)
                else do
                  let idx = ((oldFrame'^.fParseEntities) + oldIndex + 1) .&. (Constants.maxParseEntities - 1)
                  Just oldState' <- preuse $ globals.clParseEntities.ix idx
                  copyRemainingEntities showNetValue (oldState'^.esNumber) (Just oldState') (oldIndex + 1)

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

{-
- ================== CL_DeltaEntity ==================
- 
- Parses deltas from the given base and adds the resulting entity to the
- current frame
-}
deltaEntity :: Traversal' QuakeState FrameT -> Int -> EntityStateT -> Int -> Quake ()
deltaEntity frameLens newNum old bits = do
    parseEntities <- use $ globals.cl.csParseEntities
    let idx = parseEntities .&. (Constants.maxParseEntities - 1)
    globals.cl.csParseEntities += 1
    frameLens.fNumEntities += 1

    parseDelta old (globals.clParseEntities.ix idx) newNum bits

    Just state <- preuse $ globals.clParseEntities.ix idx

    preuse (globals.clEntities.ix newNum) >>= \(Just ent) ->
      when ((state^.esModelIndex) /= (ent^.ceCurrent.esModelIndex) ||
            (state^.esModelIndex2) /= (ent^.ceCurrent.esModelIndex2) ||
            (state^.esModelIndex3) /= (ent^.ceCurrent.esModelIndex3) ||
            (state^.esModelIndex4) /= (ent^.ceCurrent.esModelIndex4) ||
            abs((state^.esOrigin._x) - (ent^.ceCurrent.esOrigin._x)) > 512 ||
            abs((state^.esOrigin._y) - (ent^.ceCurrent.esOrigin._y)) > 512 ||
            abs((state^.esOrigin._z) - (ent^.ceCurrent.esOrigin._z)) > 512 ||
            (state^.esEvent) == Constants.evPlayerTeleport ||
            (state^.esEvent) == Constants.evOtherTeleport) $
        globals.clEntities.ix newNum.ceServerFrame .= -99

    Just ent <- preuse $ globals.clEntities.ix newNum
    serverFrame <- use $ globals.cl.csFrame.fServerFrame

    if (ent^.ceServerFrame) /= serverFrame - 1
      then do -- wasn't in last update, so initialize some things
        globals.clEntities.ix newNum.ceTrailCount .= 1024 -- for diminishing rocket / grenade trails
        -- duplicate the current state so lerping doesn't hurt anything
        globals.clEntities.ix newNum.cePrev .= state

        if (state^.esEvent) == Constants.evOtherTeleport
          then do
            globals.clEntities.ix newNum.cePrev.esOrigin .= (state^.esOrigin)
            globals.clEntities.ix newNum.ceLerpOrigin .= (state^.esOrigin)
          else do
            globals.clEntities.ix newNum.cePrev.esOrigin .= (state^.esOldOrigin)
            globals.clEntities.ix newNum.ceLerpOrigin .= (state^.esOldOrigin)
      else -- shuffle the last state to previous Copy !
        globals.clEntities.ix newNum.cePrev .= (ent^.ceCurrent)

    globals.clEntities.ix newNum.ceServerFrame .= serverFrame
    -- Copy !
    globals.clEntities.ix newNum.ceCurrent .= state

{-
- =============== CL_CalcViewValues ===============
- 
- Sets cl.refdef view values
-}
calcViewValues :: Quake ()
calcViewValues = do
    -- find the previous frame to interpolate from
    cl' <- use $ globals.cl
    let ps = cl'^.csFrame.fPlayerState
        i = ((cl'^.csFrame.fServerFrame) - 1) .&. Constants.updateMask
        oldFrame = (cl'^.csFrames) V.! i
        oldFrame' = if (oldFrame^.fServerFrame) /= (cl'^.csFrame.fServerFrame) - 1 || not (oldFrame^.fValid)
                      then cl'^.csFrame -- previous frame was dropped or invalid
                      else oldFrame
        ops = oldFrame'^.fPlayerState

        -- see if the player entity was teleported this frame
        ops' = if abs ((ops^.psPMoveState.pmsOrigin._x) - (ps^.psPMoveState.pmsOrigin._x)) > 256 * 8 ||
                  abs ((ops^.psPMoveState.pmsOrigin._y) - (ps^.psPMoveState.pmsOrigin._y)) > 256 * 8 ||
                  abs ((ops^.psPMoveState.pmsOrigin._z) - (ps^.psPMoveState.pmsOrigin._z)) > 256 * 8
                  then ps -- don't interpolate
                  else ops

        lerp = cl'^.csLerpFrac

    -- calculate the origin
    predictValue <- liftM (^.cvValue) clPredictCVar
    if predictValue /= 0 && (cl'^.csFrame.fPlayerState.psPMoveState.pmsPMFlags) .&. pmfNoPrediction == 0 -- use predicted values
      then do
        let backlerp = 1 - lerp
        globals.cl.csRefDef.rdViewOrg .= (cl'^.csPredictedOrigin)
                                       + (ops'^.psViewOffset)
                                       + (fmap (* (cl'^.csLerpFrac)) ((ps^.psViewOffset) - (ops'^.psViewOffset)))
                                       - (fmap (* backlerp) (cl'^.csPredictionError))

        -- smooth out stair climbing
        realTime <- use $ globals.cls.csRealTime
        let delta = (realTime - (cl'^.csPredictedStepTime))
        when (delta < 100) $
          globals.cl.csRefDef.rdViewOrg._z -= (cl'^.csPredictedStep) * fromIntegral (100 - delta) * 0.01

      else do -- juse use interpolated values
        let v = (fmap ((* 0.125) . fromIntegral) (ps^.psPMoveState.pmsOrigin))
              + (ps^.psViewOffset)
              - ((fmap ((* 0.125) . fromIntegral) (ops'^.psPMoveState.pmsOrigin)) + (ops'^.psViewOffset))
        globals.cl.csRefDef.rdViewOrg .= (fmap ((* 0.125) . fromIntegral) (ops'^.psPMoveState.pmsOrigin))
                                       + (ops'^.psViewOffset)
                                       + (fmap (* lerp) v)

    -- if not running a demo or on a locked frame, add the local angle
    -- movement
    if (cl'^.csFrame.fPlayerState.psPMoveState.pmsPMType) < Constants.pmDead
      then globals.cl.csRefDef.rdViewAngles .= (cl'^.csPredictedAngles) -- use predicted values
      else globals.cl.csRefDef.rdViewAngles .= Math3D.lerpAngles (ops'^.psViewAngles) (ps^.psViewAngles) lerp -- just use interpolated values

    globals.cl.csRefDef.rdViewAngles += Math3D.lerpAngles (ops'^.psKickAngles) (ps^.psKickAngles) lerp

    rd <- use $ globals.cl.csRefDef
    let (Just f, Just r, Just u) = Math3D.angleVectors (rd^.rdViewAngles) True True True
    globals.cl.csVForward .= f
    globals.cl.csVRight .= r
    globals.cl.csVUp .= u

    -- interpolate field of view
    globals.cl.csRefDef.rdFovX .= (ops'^.psFOV) + lerp * ((ps^.psFOV) - (ops'^.psFOV))

    -- don't interpolate blend color
    globals.cl.csRefDef.rdBlend .= (ps^.psBlend)

    -- add the weapon
    addViewWeapon ps ops'

addViewWeapon :: PlayerStateT -> PlayerStateT -> Quake ()
addViewWeapon ps ops = do
    clGunValue <- liftM (^.cvValue) clGunCVar

    -- allow the gun to be completely removed
    -- don't draw gun if in wide angle view
    unless (clGunValue == 0 || (ps^.psFOV) > 90) $ do
      gunModel' <- use $ globals.gunModel

      model <- case gunModel' of
                 Nothing -> preuse (globals.cl.csModelDraw.ix (ps^.psGunIndex)) >>= \(Just m) -> return m
                 Just m -> return gunModel' -- development tool

      case model of
        Nothing -> return ()
        Just _ -> do
          -- set up gun position
          cl' <- use $ globals.cl
          let origin = (cl'^.csRefDef.rdViewOrg)
                     + (ops^.psGunOffset)
                     + (fmap (* (cl'^.csLerpFrac)) ((ps^.psGunOffset) - (ops^.psGunOffset)))

              angles = (cl'^.csRefDef.rdViewAngles)
                     + (Math3D.lerpAngles (ops^.psGunAngles) (ps^.psGunAngles) (cl'^.csLerpFrac))

          gunFrame' <- use $ globals.gunFrame
          let (frame, oldFrame) = if gunFrame' /= 0
                                    then (gunFrame', gunFrame') -- development tool
                                    else if (ps^.psGunFrame) == 0
                                           then (0, 0) -- just changed weapons, don't lerp from old
                                           else (ps^.psGunFrame, ops^.psGunFrame)

              gun = newEntityT { _eModel = model
                               , _eAngles = angles
                               , _eOrigin = origin
                               , _eFrame = frame
                               , _eOldOrigin = origin
                               , _eOldFrame = oldFrame
                               , _eBackLerp = 1 - (cl'^.csLerpFrac)
                               , _enFlags = Constants.rfMinLight .|. Constants.rfDepthHack .|. Constants.rfWeaponModel
                               }

          ClientV.addEntity gun

addPacketEntities :: FrameT -> Quake ()
addPacketEntities frame = do
    cl' <- use $ globals.cl
        -- bonus items rotate at a fixed rate
    let autoRotate = Math3D.angleMod (fromIntegral (cl'^.csTime) / 10)
        -- brush models can auto animate their frames
        autoAnim = 2 * (cl'^.csTime) `div` 1000

    addEntity autoRotate autoAnim newEntityT 0 (frame^.fNumEntities)

  where addEntity :: Float -> Int -> EntityT -> Int -> Int -> Quake ()
        addEntity autoRotate autoAnim ent pNum maxPNum
          | pNum >= maxPNum = return ()
          | otherwise = do
              Just s1 <- preuse $ globals.clParseEntities.ix (((frame^.fParseEntities) + pNum) .&. (Constants.maxParseEntities - 1))
              Just cent <- preuse $ globals.clEntities.ix (s1^.esNumber)

              cl' <- use $ globals.cl
              let entFrame = setFrame autoAnim s1 (cl'^.csTime)
                  (effects, renderfx) = calcEffectsAndRenderFx s1
                  endOldFrame = cent^.cePrev.esFrame
                  entBackLerp = 1 - (cl'^.csLerpFrac)
                  (entOrigin, entOldOrigin) = calcOrigin cent renderfx (cl'^.csLerpFrac)

              (entAlpha, entSkinNum, entSkin, entModel) <- tweakBeamsColor cl' s1 ent renderfx

                  -- only used for black hole model right now, FIXME: do better
              let entAlpha' = if renderfx == Constants.rfTranslucent
                                then 0.7 else entAlpha
                  -- render effects (fullbright, translucent, etc)
                  entFlags = if effects .&. Constants.efColorShell /= 0
                               then 0 -- renderfx go on color shell entity
                               else renderfx
              entAngles <- calcAngles cl' s1 ent cent autoRotate effects

              io (putStrLn "CLEnts.addPacketEntities") >> undefined -- TODO

        setFrame :: Int -> EntityStateT -> Int -> Int
        setFrame autoAnim s1 time =
          let effects = s1^.esEffects
          in if | effects .&. Constants.efAnim01 /= 0 -> autoAnim .&. 1
                | effects .&. Constants.efAnim23 /= 0 -> 2 + (autoAnim .&. 1)
                | effects .&. Constants.efAnimAll /= 0 -> autoAnim
                | effects .&. Constants.efAnimAllFast /= 0 -> time `div` 100
                | otherwise -> s1^.esFrame

        calcEffectsAndRenderFx :: EntityStateT -> (Int, Int)
        calcEffectsAndRenderFx s1 =
          let effects = s1^.esEffects
              renderfx = s1^.esRenderFx
              (effects', renderfx') = if effects .&. Constants.efPent /= 0
                                        then ((effects .&. (complement Constants.efPent)) .|. Constants.efColorShell, renderfx .|. Constants.rfShellRed)
                                        else (effects, renderfx)
              (effects'', renderfx'') = if effects' .&. Constants.efQuad /= 0
                                          then ((effects' .&. (complement Constants.efQuad)) .|. Constants.efColorShell, renderfx' .|. Constants.rfShellBlue)
                                          else (effects', renderfx')
              (effects''', renderfx''') = if effects'' .&. Constants.efDouble /= 0
                                            then ((effects'' .&. (complement Constants.efDouble)) .|. Constants.efColorShell, renderfx'' .|. Constants.rfShellDouble)
                                            else (effects'', renderfx'')
              result = if effects''' .&. Constants.efHalfDamage /= 0
                         then ((effects''' .&. (complement Constants.efHalfDamage)) .|. Constants.efColorShell, renderfx''' .|. Constants.rfShellHalfDam)
                         else (effects''', renderfx''')
          in result

        calcOrigin :: CEntityT -> Int -> Float -> (V3 Float, V3 Float)
        calcOrigin cent renderfx lerpFrac =
          if renderfx .&. (Constants.rfFrameLerp .|. Constants.rfBeam) /= 0
            then -- step origin discretely, because the frames
                 -- do the animation properly
              (cent^.ceCurrent.esOrigin, cent^.ceCurrent.esOldOrigin)
            else -- interpolate origin
              let v = (cent^.cePrev.esOrigin)
                    + (fmap (* lerpFrac) ((cent^.ceCurrent.esOrigin) - (cent^.cePrev.esOrigin)))
              in (v, v)

        tweakBeamsColor :: ClientStateT -> EntityStateT -> EntityT -> Int -> Quake (Float, Int, Maybe ImageReference, Maybe ModelReference)
        tweakBeamsColor cl' s1 ent renderfx = do
          if renderfx .&. Constants.rfBeam /= 0 -- the four beam colors are encoded in 32 bits of skinnum (hack)
            then do
              r <- liftM (`mod` 4) Lib.rand
              return (0.3, ((s1^.esSkinNum) `shiftR` (fromIntegral r * 8)) .&. 0xFF, ent^.eSkin, Nothing)
            else do
              -- set skin
              if (s1^.esModelIndex) == 255 -- use custom player skin
                then do
                  let skinNum = 0
                      ci = (cl'^.csClientInfo) V.! ((s1^.esSkinNum) .&. 0xFF)
                      (skin, model) = if isNothing (ci^.ciSkin) || isNothing (ci^.ciModel)
                                        then (cl'^.csBaseClientInfo.ciSkin, cl'^.csBaseClientInfo.ciModel)
                                        else (ci^.ciSkin, ci^.ciModel)

                  (skin', model') <- if renderfx .&. Constants.rfUseDisguise /= 0
                                       then do
                                         Just renderer <- use $ globals.re
                                         let registerSkin = renderer^.rRefExport.reRegisterSkin
                                             registerModel = renderer^.rRefExport.reRegisterModel

                                         Just image <- (renderer^.rRefExport.reGetImage) (fromJust skin)

                                         if | "players/male" `BC.isPrefixOf` (image^.iName) -> do
                                                s <- registerSkin "players/male/disguise.pcx"
                                                m <- registerModel "players/male/tris.md2"
                                                return (s, m)
                                            | "players/female" `BC.isPrefixOf` (image^.iName) -> do
                                                s <- registerSkin "players/female/disguise.pcx"
                                                m <- registerModel "players/female/tris.md2"
                                                return (s, m)
                                            | "players/cyborg" `BC.isPrefixOf` (image^.iName) -> do
                                                s <- registerSkin "players/cyborg/disguise.pcx"
                                                m <- registerModel "players/cyborg/tris.md2"
                                                return (s, m)
                                            | otherwise ->
                                                return (skin, model)
                                       else
                                         return (skin, model)

                  return (ent^.eAlpha, skinNum, skin', model')
                else
                  return (ent^.eAlpha, s1^.esSkinNum, Nothing, (cl'^.csModelDraw) V.! (s1^.esModelIndex))

        calcAngles :: ClientStateT -> EntityStateT -> EntityT -> CEntityT -> Float -> Int -> Quake (V3 Float)
        calcAngles cl' s1 ent cent autoRotate effects = do
          if | effects .&. Constants.efRotate /= 0 -> -- some bonus items
                 return (V3 0 autoRotate 0)

               -- RAFAEL
             | effects .&. Constants.efSpinningLights /= 0 -> do
                 let result = V3 0 (Math3D.angleMod (fromIntegral (cl'^.csTime) / 2) + (s1^.esAngles._y)) 180
                     (Just forward, _, _) = Math3D.angleVectors (ent^.eAngles) True False False
                     start = (ent^.eOrigin) + fmap (* 64) forward
                 ClientV.addLight start 100 1 0 0
                 return result

               -- interpolate angles
             | otherwise ->
                 return (Math3D.lerpAngles (cent^.cePrev.esAngles) (cent^.ceCurrent.esAngles) (cl'^.csLerpFrac))
