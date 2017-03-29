{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Client.CLInput where

import Control.Lens ((^.), use, ix, (.=), preuse, Lens', (%=), (-=), (+=), (*=), zoom, _1, _2)
import Control.Monad (void, unless, when, liftM)
import Data.Bits ((.&.), xor, (.|.), complement)
import Linear (_x, _y, _z)
import qualified Data.ByteString as B
import qualified Data.Vector as V

import Client.FrameT
import Game.GClientT
import Game.MoveInfoT
import Game.ClientPersistantT
import Game.ClientRespawnT
import Game.MonsterInfoT
import Game.PlayerStateT
import Types
import Game.PMoveStateT
import QuakeState
import CVarVariables
import QCommon.XCommandT
import qualified Constants
import qualified Client.CL as CL
import qualified Client.SCR as SCR
import {-# SOURCE #-} qualified Game.Cmd as Cmd
import qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import qualified QCommon.MSG as MSG
import qualified QCommon.NetChannel as NetChannel
import qualified QCommon.SZ as SZ
import qualified Sys.IN as IN
import qualified Sys.Timer as Timer
import qualified Util.Lib as Lib
import qualified Util.Math3D as Math3D

nullcmd :: UserCmdT
nullcmd = newUserCmdT

initInput :: Quake()
initInput = do
    Cmd.addCommand "centerview" (Just IN.centerView)
    Cmd.addCommand "+moveup" (Just upDown)
    Cmd.addCommand "-moveup" (Just upUp)
    Cmd.addCommand "+movedown" (Just downDown)
    Cmd.addCommand "-movedown" (Just downUp)
    Cmd.addCommand "+left" (Just leftDown)
    Cmd.addCommand "-left" (Just leftUp)
    Cmd.addCommand "+right" (Just rightDown)
    Cmd.addCommand "-right" (Just rightUp)
    Cmd.addCommand "+forward" (Just forwardDown)
    Cmd.addCommand "-forward" (Just forwardUp)
    Cmd.addCommand "+back" (Just backDown)
    Cmd.addCommand "-back" (Just backUp)
    Cmd.addCommand "+lookup" (Just lookUpDown)
    Cmd.addCommand "-lookup" (Just lookUpUp)
    Cmd.addCommand "+lookdown" (Just lookDownDown)
    Cmd.addCommand "-lookdown" (Just lookDownUp)
    Cmd.addCommand "+strafe" (Just strafeDown)
    Cmd.addCommand "-strafe" (Just strafeUp)
    Cmd.addCommand "+moveleft" (Just moveLeftDown)
    Cmd.addCommand "-moveleft" (Just moveLeftUp)
    Cmd.addCommand "+moveright" (Just moveRightDown)
    Cmd.addCommand "-moveright" (Just moveRightUp)
    Cmd.addCommand "+speed" (Just speedDown)
    Cmd.addCommand "-speed" (Just speedUp)
    Cmd.addCommand "+attack" (Just attackDown)
    Cmd.addCommand "-attack" (Just attackUp)
    Cmd.addCommand "+use" (Just useDown)
    Cmd.addCommand "-use" (Just useUp)
    Cmd.addCommand "impulse" (Just impulse)
    Cmd.addCommand "+klook" (Just kLookDown)
    Cmd.addCommand "-klook" (Just kLookUp)

    void $ CVar.get "cl_nodelta" "0" 0

upDown :: XCommandT
upDown = XCommandT "CLInput.upDown" (inputKeyDown (clientGlobals.cgInUp))

upUp :: XCommandT
upUp = XCommandT "CLInput.upUp" (inputKeyUp (clientGlobals.cgInUp))

downDown :: XCommandT
downDown = XCommandT "CLInput.downDown" (inputKeyDown (clientGlobals.cgInDown))

downUp :: XCommandT
downUp = XCommandT "CLInput.downUp" (inputKeyUp (clientGlobals.cgInDown))

leftDown :: XCommandT
leftDown = XCommandT "CLInput.leftDown" (inputKeyDown (clientGlobals.cgInLeft))

leftUp :: XCommandT
leftUp = XCommandT "CLInput.leftUp" (inputKeyUp (clientGlobals.cgInLeft))

rightDown :: XCommandT
rightDown = XCommandT "CLInput.rightDown" (inputKeyDown (clientGlobals.cgInRight))

rightUp :: XCommandT
rightUp = XCommandT "CLInput.rightUp" (inputKeyUp (clientGlobals.cgInRight))

forwardDown :: XCommandT
forwardDown = XCommandT "CLInput.forwardDown" (inputKeyDown (clientGlobals.cgInForward))

forwardUp :: XCommandT
forwardUp = XCommandT "CLInput.forwardUp" (inputKeyUp (clientGlobals.cgInForward))

backDown :: XCommandT
backDown = XCommandT "CLInput.backDown" (inputKeyDown (clientGlobals.cgInBack))

backUp :: XCommandT
backUp = XCommandT "CLInput.backUp" (inputKeyUp (clientGlobals.cgInBack))

lookUpDown :: XCommandT
lookUpDown = XCommandT "CLInput.lookUpDown" (inputKeyDown (clientGlobals.cgInLookUp))

lookUpUp :: XCommandT
lookUpUp = XCommandT "CLInput.lookUpUp" (inputKeyUp (clientGlobals.cgInLookUp))

lookDownDown :: XCommandT
lookDownDown = XCommandT "CLInput.lookDownDown" (inputKeyDown (clientGlobals.cgInLookDown))

lookDownUp :: XCommandT
lookDownUp = XCommandT "CLInput.lookDownUp" (inputKeyUp (clientGlobals.cgInLookDown))

strafeDown :: XCommandT
strafeDown = XCommandT "CLInput.strafeDown" (inputKeyDown (clientGlobals.cgInStrafe))

strafeUp :: XCommandT
strafeUp = XCommandT "CLInput.strafeUp" (inputKeyUp (clientGlobals.cgInStrafe))

moveLeftDown :: XCommandT
moveLeftDown = XCommandT "CLInput.moveLeftDown" (inputKeyDown (clientGlobals.cgInMoveLeft))

moveLeftUp :: XCommandT
moveLeftUp = XCommandT "CLInput.moveLeftUp" (inputKeyUp (clientGlobals.cgInMoveLeft))

moveRightDown :: XCommandT
moveRightDown = XCommandT "CLInput.moveRightDown" (inputKeyDown (clientGlobals.cgInMoveRight))

moveRightUp :: XCommandT
moveRightUp = XCommandT "CLInput.moveRightUp" (inputKeyUp (clientGlobals.cgInMoveRight))

speedDown :: XCommandT
speedDown = XCommandT "CLInput.speedDown" (inputKeyDown (clientGlobals.cgInSpeed))

speedUp :: XCommandT
speedUp = XCommandT "CLInput.speedUp" (inputKeyUp (clientGlobals.cgInSpeed))

attackDown :: XCommandT
attackDown = XCommandT "CLInput.attackDown" (inputKeyDown (clientGlobals.cgInAttack))

attackUp :: XCommandT
attackUp = XCommandT "CLInput.attackUp" (inputKeyUp (clientGlobals.cgInAttack))

useDown :: XCommandT
useDown = XCommandT "CLInput.useDown" (inputKeyDown (clientGlobals.cgInUse))

useUp :: XCommandT
useUp = XCommandT "CLInput.useUp" (inputKeyUp (clientGlobals.cgInUse))

impulse :: XCommandT
impulse =
  XCommandT "CLInput.impulse" (do
    v1 <- Cmd.argv 1
    clientGlobals.cgInImpulse .= Lib.atoi v1
  )

kLookDown :: XCommandT
kLookDown = XCommandT "CLInput.kLookDown" (inputKeyDown (clientGlobals.cgInKLook))

kLookUp :: XCommandT
kLookUp = XCommandT "CLInput.kLookUp" (inputKeyUp (clientGlobals.cgInKLook))

sendCmd :: Quake ()
sendCmd = do
    -- build a command even if not connected

    -- save this command off for prediction
    cmdRef <- saveCommandForPrediction

    state <- use $ globals.cls.csState

    unless (state == Constants.caDisconnected || state == Constants.caConnecting) $ do
      curSize <- use $ globals.cls.csNetChan.ncMessage.sbCurSize
      lastSent <- use $ globals.cls.csNetChan.ncLastSent
      curTime <- Timer.getCurTime

      if state == Constants.caConnected
        then
          when (curSize /= 0 || (curTime - lastSent) > 1000) $
            NetChannel.transmit (globals.cls.csNetChan) 0 ""
        else do
          -- send a userinfo update if needed
          use (globals.userInfoModified) >>= \modified ->
            when modified $ do
              CL.fixUpGender
              globals.userInfoModified .= False
              MSG.writeByteI (globals.cls.csNetChan.ncMessage) (fromIntegral Constants.clcUserInfo)
              userInfo <- CVar.userInfo
              MSG.writeString (globals.cls.csNetChan.ncMessage) userInfo

          SZ.init (clientGlobals.cgBuf) "" 128

          shouldSkipCinematic cmdRef >>= \shouldSkip ->
            -- skip the rest of the cinematic
            when shouldSkip SCR.finishCinematic

          -- begin a client move command
          MSG.writeByteI (clientGlobals.cgBuf) (fromIntegral Constants.clcMove)

          -- save the position for a checksum byte
          checksumIndex <- use $ clientGlobals.cgBuf.sbCurSize
          MSG.writeByteI (clientGlobals.cgBuf) 0

          -- let the server know what the last frame we
          -- got was, so the next message can be delta compressed
          setCompressionInfo

          -- send this and the previous cmds in the message, so
          -- if the last packet was dropped, it can be recovered
          writeCommandsToBuf

          -- IMPROVE: this is most likely a candidate for performance optimization
          -- calculate a checksum over the move commands
          calculateChecksum checksumIndex

          -- deliver the message
          deliverMessage

  where saveCommandForPrediction :: Quake UserCmdReference
        saveCommandForPrediction = do
          cls' <- use $ globals.cls
          let i = (cls'^.csNetChan.ncOutgoingSequence) .&. (Constants.cmdBackup - 1)
              cmdRef = UserCmdReference i
          realTime <- use $ globals.cls.csRealTime
          globals.cl.csCmdTime.ix i .= realTime -- for netgraph ping calculation

          -- fill the cmd
          createCmd cmdRef

          Just cmd <- preuse $ globals.cl.csCmds.ix i
          globals.cl.csCmd .= cmd

          return cmdRef

        shouldSkipCinematic :: UserCmdReference -> Quake Bool
        shouldSkipCinematic (UserCmdReference idx) = do
          Just cmd <- preuse $ globals.cl.csCmds.ix idx

          realTime <- use $ globals.cls.csRealTime
          cl' <- use $ globals.cl

          return $ if (cmd^.ucButtons) /= 0 && (cl'^.csCinematicTime) > 0 &&
                      not (cl'^.csAttractLoop) && realTime - (cl'^.csCinematicTime) > 1000
                     then True
                     else False

        setCompressionInfo :: Quake ()
        setCompressionInfo = do
          noDeltaValue <- liftM (^.cvValue) clNoDeltaCVar
          validFrame <- use $ globals.cl.csFrame.fValid
          demoWaiting <- use $ globals.cls.csDemoWaiting

          if noDeltaValue /= 0 || not validFrame || demoWaiting
            then 
              MSG.writeLong (clientGlobals.cgBuf) (-1) -- no compression
            else do
              serverFrame <- use $ globals.cl.csFrame.fServerFrame
              MSG.writeLong (clientGlobals.cgBuf) serverFrame

        writeCommandsToBuf :: Quake ()
        writeCommandsToBuf = do
          outgoingSequence <- use $ globals.cls.csNetChan.ncOutgoingSequence
          cmds <- use $ globals.cl.csCmds

          let i = (outgoingSequence - 2) .&. (Constants.cmdBackup - 1)
          MSG.writeDeltaUserCmd (clientGlobals.cgBuf) nullcmd (cmds V.! i)

          let j = (outgoingSequence - 1) .&. (Constants.cmdBackup - 1)
          MSG.writeDeltaUserCmd (clientGlobals.cgBuf) (cmds V.! i) (cmds V.! j)

          let k = outgoingSequence .&. (Constants.cmdBackup - 1)
          MSG.writeDeltaUserCmd (clientGlobals.cgBuf) (cmds V.! j) (cmds V.! k)

        deliverMessage :: Quake ()
        deliverMessage = do
          buf <- use $ clientGlobals.cgBuf
          io $ print "CLInput.sendCmd"
          NetChannel.transmit (globals.cls.csNetChan) (buf^.sbCurSize) (buf^.sbData)

        calculateChecksum :: Int -> Quake ()
        calculateChecksum checksumIndex = do
          buf <- use $ clientGlobals.cgBuf
          outgoingSequence <- use $ globals.cls.csNetChan.ncOutgoingSequence
          crcByte <- Com.blockSequenceCRCByte (buf^.sbData) (checksumIndex + 1) ((buf^.sbCurSize) - checksumIndex - 1) outgoingSequence
          clientGlobals.cgBuf.sbData .= ((B.take checksumIndex (buf^.sbData)) `B.snoc` crcByte) `B.append` (B.drop (checksumIndex + 1) (buf^.sbData))

createCmd :: UserCmdReference -> Quake ()
createCmd cmdRef = do
    sysFrameTime' <- use $ globals.sysFrameTime
    oldSysFrameTime <- use $ clientGlobals.cgOldSysFrameTime

    let diff = fromIntegral sysFrameTime' - oldSysFrameTime
        frameMsec = if | diff < 1 -> 1
                       | diff > 200 -> 200
                       | otherwise -> diff

    clientGlobals.cgFrameMsec .= fromIntegral frameMsec

    -- get basic movement from keyboard
    baseMove cmdRef

    -- allow mice or other external controllers to add to the move
    IN.move cmdRef

    finishMove cmdRef

    use (globals.sysFrameTime) >>= \v ->
      clientGlobals.cgOldSysFrameTime .= fromIntegral v

{-
- ================ CL_BaseMove ================
- 
- Send the intended movement message to the server
-}
baseMove :: UserCmdReference -> Quake ()
baseMove (UserCmdReference cmdIdx) = do
    adjustAngles

    globals.cl.csCmds.ix cmdIdx .= newUserCmdT

    use (globals.cl.csViewAngles) >>= \angles ->
      globals.cl.csCmds.ix cmdIdx.ucAngles .= fmap truncate angles

    inStrafe <- use $ clientGlobals.cgInStrafe
    inKLook <- use $ clientGlobals.cgInKLook
    inSpeed <- use $ clientGlobals.cgInSpeed

    sideSpeedValue <- liftM (^.cvValue) clSideSpeedCVar
    upSpeedValue <- liftM (^.cvValue) clUpSpeedCVar
    forwardSpeedValue <- liftM (^.cvValue) clForwardSpeedCVar
    runValue <- liftM (^.cvValue) clRunCVar

    when ((inStrafe^.kbState) .&. 1 /= 0) $ do
      rightValue <- keyState (clientGlobals.cgInRight)
      leftValue <- keyState (clientGlobals.cgInLeft)
      zoom (globals.cl.csCmds.ix cmdIdx) $ do
        ucSideMove += truncate (sideSpeedValue * rightValue)
        ucSideMove -= truncate (sideSpeedValue * leftValue)

    moveRightValue <- keyState (clientGlobals.cgInMoveRight)
    moveLeftValue <- keyState (clientGlobals.cgInMoveLeft)
    zoom (globals.cl.csCmds.ix cmdIdx) $ do
      ucSideMove += truncate (sideSpeedValue * moveRightValue)
      ucSideMove -= truncate (sideSpeedValue * moveLeftValue)

    upValue <- keyState (clientGlobals.cgInUp)
    downValue <- keyState (clientGlobals.cgInDown)
    zoom (globals.cl.csCmds.ix cmdIdx) $ do
      ucUpMove += truncate (upSpeedValue * upValue)
      ucUpMove -= truncate (upSpeedValue * downValue)

    when ((inKLook^.kbState) .&. 1 == 0) $ do
      forwardValue <- keyState (clientGlobals.cgInForward)
      backValue <- keyState (clientGlobals.cgInBack)
      zoom (globals.cl.csCmds.ix cmdIdx) $ do
        ucForwardMove += truncate (forwardSpeedValue * forwardValue)
        ucForwardMove -= truncate (forwardSpeedValue * backValue)

    -- adjust for speed key / running
    when (((inSpeed^.kbState) .&. 1) `xor` (truncate runValue) /= 0) $ do
      zoom (globals.cl.csCmds.ix cmdIdx) $ do
        ucForwardMove *= 2
        ucSideMove *= 2
        ucUpMove *= 2

finishMove :: UserCmdReference -> Quake ()
finishMove (UserCmdReference cmdIdx) = do
    -- figure button bits
    use (clientGlobals.cgInAttack.kbState) >>= \v ->
      when (v .&. 3 /= 0) $
        globals.cl.csCmds.ix cmdIdx.ucButtons %= (.|. (fromIntegral Constants.buttonAttack))

    clientGlobals.cgInAttack.kbState %= (.&. (complement 2))

    use (clientGlobals.cgInUse.kbState) >>= \v ->
      when (v .&. 3 /= 0) $
        globals.cl.csCmds.ix cmdIdx.ucButtons %= (.|. (fromIntegral Constants.buttonUse))

    clientGlobals.cgInUse.kbState %= (.&. (complement 2))

    anyKeyDown <- use $ keyGlobals.kgAnyKeyDown
    keyDest <- use $ globals.cls.csKeyDest

    when (anyKeyDown /= 0 && keyDest == Constants.keyGame) $
      globals.cl.csCmds.ix cmdIdx.ucButtons %= (.|. (fromIntegral Constants.buttonAny))

    -- send milliseconds of time to apply the move
    frameTime <- use $ globals.cls.csFrameTime
    let ms :: Int = truncate (frameTime * 1000)
        ms' = if ms > 250
                then 100 -- time was unreasonable
                else ms

    globals.cl.csCmds.ix cmdIdx.ucMsec .= fromIntegral ms'

    clampPitch

    viewAngles <- use $ globals.cl.csViewAngles
    globals.cl.csCmds.ix cmdIdx.ucAngles .= fmap Math3D.angleToShort viewAngles

    inImpulse <- use $ clientGlobals.cgInImpulse
    globals.cl.csCmds.ix cmdIdx.ucImpulse .= fromIntegral inImpulse
    clientGlobals.cgInImpulse .= 0

    -- send the ambient light level at the player's current position
    lightLevelValue <- liftM (^.cvValue) clLightLevelCVar
    globals.cl.csCmds.ix cmdIdx.ucLightLevel .= truncate lightLevelValue

{-
- ================ CL_AdjustAngles ================
- 
- Moves the local angle positions
-}
adjustAngles :: Quake ()
adjustAngles = do
    inSpeed <- use $ clientGlobals.cgInSpeed
    inStrafe <- use $ clientGlobals.cgInStrafe
    inKLook <- use $ clientGlobals.cgInKLook
    frameTime <- use $ globals.cls.csFrameTime
    pitchSpeed <- liftM (^.cvValue) clPitchSpeedCVar

    speed <- if (inSpeed^.kbState) .&. 1 /= 0
               then do
                 angleSpeedKeyValue <- liftM (^.cvValue) clAngleSpeedkeyCVar
                 return $ frameTime * angleSpeedKeyValue
               else
                 return frameTime

    when ((inStrafe^.kbState) .&. 1 == 0) $ do
      yawSpeed <- liftM (^.cvValue) clYawSpeedCVar
      rightValue <- keyState (clientGlobals.cgInRight)
      leftValue <- keyState (clientGlobals.cgInLeft)
      let access = case Constants.yaw of
                     0 -> _x
                     1 -> _y
                     2 -> _z
                     _ -> undefined -- shouldn't happen
      globals.cl.csViewAngles.access -= speed * yawSpeed * rightValue
      globals.cl.csViewAngles.access += speed * yawSpeed * leftValue

    when ((inKLook^.kbState) .&. 1 /= 0) $ do
      forwardValue <- keyState (clientGlobals.cgInForward)
      backValue <- keyState (clientGlobals.cgInBack)
      let access = case Constants.pitch of
                     0 -> _x
                     1 -> _y
                     2 -> _z
                     _ -> undefined -- shouldn't happen
      globals.cl.csViewAngles.access -= speed * pitchSpeed * forwardValue
      globals.cl.csViewAngles.access += speed * pitchSpeed * backValue

    upValue <- keyState (clientGlobals.cgInLookUp)
    downValue <- keyState (clientGlobals.cgInLookDown)
    let access = case Constants.pitch of
                   0 -> _x
                   1 -> _y
                   2 -> _z
                   _ -> undefined -- shouldn't happen
    globals.cl.csViewAngles.access -= speed * pitchSpeed * upValue
    globals.cl.csViewAngles.access += speed * pitchSpeed * downValue

keyState :: Lens' QuakeState KButtonT -> Quake Float
keyState keyLens = do
    -- clear impulses
    keyLens.kbState %= (.&. 1)

    key <- use keyLens

    let msec = key^.kbMsec
    keyLens.kbMsec .= 0

    msec' <- if (key^.kbState) /= 0
               then do
                 -- still down
                 sysFrameTime' <- liftM fromIntegral (use $ globals.sysFrameTime)

                 let ms = msec + sysFrameTime' - (key^.kbDownTime)
                 keyLens.kbDownTime .= sysFrameTime'
                 return ms
               else
                 return msec

    frameMsec <- use $ clientGlobals.cgFrameMsec
    let val :: Float = fromIntegral msec' / fromIntegral frameMsec

    return $ if | val < 0 -> 0
                | val > 1 -> 1
                | otherwise -> val

clampPitch :: Quake ()
clampPitch = do
    let access = case Constants.pitch of
                   0 -> _x
                   1 -> _y
                   2 -> _z
                   _ -> undefined -- shouldn't happen

    angle <- use $ globals.cl.csFrame.fPlayerState.psPMoveState.pmsDeltaAngles.access
    let p = Math3D.shortToAngle (fromIntegral angle)
        pitch = if p > 180
                  then p - 360
                  else p

    let access2 = case Constants.pitch of
                    0 -> _x
                    1 -> _y
                    2 -> _z
                    _ -> undefined -- shouldn't happen

    use (globals.cl.csViewAngles.(Math3D.v3Access Constants.pitch)) >>= \v ->
      when (v + pitch < (-360)) $
        globals.cl.csViewAngles.access2 += 360 -- wrapped

    use (globals.cl.csViewAngles.(Math3D.v3Access Constants.pitch)) >>= \v ->
      when (v + pitch > 360) $
        globals.cl.csViewAngles.access2 -= 360 -- wrapped

    use (globals.cl.csViewAngles.(Math3D.v3Access Constants.pitch)) >>= \v ->
      when (v + pitch > 89) $
        globals.cl.csViewAngles.access2 .= 89 - pitch

    use (globals.cl.csViewAngles.(Math3D.v3Access Constants.pitch)) >>= \v ->
      when (v + pitch < (-89)) $
        globals.cl.csViewAngles.access2 .= (-89) - pitch

inputKeyDown :: Lens' QuakeState KButtonT -> Quake ()
inputKeyDown buttonLens = do
    b <- use buttonLens
    c <- Cmd.argv 1

    let k = if B.length c > 0
              then Lib.atoi c
              else -1 -- typed manually at the console for continuous down

    unless ((b^.kbDown._1) == k || (b^.kbDown._2) == k) $ do -- repeating key
      done <- if | (b^.kbDown._1) == 0 -> do
                     buttonLens.kbDown._1 .= k
                     return False
                 | (b^.kbDown._2) == 0 -> do
                     buttonLens.kbDown._2 .= k
                     return False
                 | otherwise -> do
                     Com.printf "Three keys down for a button!\n"
                     return True

      unless done $ do
        unless ((b^.kbState) .&. 1 /= 0) $ do -- unless still down
          -- save timestamp
          t <- Cmd.argv 2
          let downtime = Lib.atoi t
          buttonLens.kbDownTime .= fromIntegral downtime

          when (downtime == 0) $ do
            frameTime <- use $ globals.sysFrameTime
            buttonLens.kbDownTime .= fromIntegral (frameTime - 100)

          buttonLens.kbState %= (.|. 3) -- down + impulse down

inputKeyUp :: Lens' QuakeState KButtonT -> Quake ()
inputKeyUp buttonLens = do
    c <- Cmd.argv 1

    if B.length c <= 0
      then do
        -- typed manually at the console, assume for unsticking, so clear
        -- all
        buttonLens.kbDown .= (0, 0)
        buttonLens.kbState .= 4 -- impulse up
      else do
        let k = Lib.atoi c

        b <- use buttonLens

        done <- if | (b^.kbDown._1) == k -> do
                       buttonLens.kbDown._1 .= 0
                       return False
                   | (b^.kbDown._2) == k -> do
                       buttonLens.kbDown._2 .= 0
                       return False
                   | otherwise -> return True -- key up without coresponding down (menu pass through)

        skip <- shouldSkip

        unless (done || skip) $ do
          -- save timestamp
          t <- Cmd.argv 2
          let uptime = Lib.atoi t

          if uptime /= 0
            then buttonLens.kbMsec += fromIntegral uptime - (b^.kbDownTime)
            else buttonLens.kbMsec += 10

          buttonLens.kbState %= (.&. (complement 1)) -- now up
          buttonLens.kbState %= (.|. 4) -- impulse up

  where shouldSkip :: Quake Bool
        shouldSkip = do
          b <- use buttonLens
          if | (b^.kbDown._1) /= 0 || (b^.kbDown._2) /= 0 -> return True -- some other key is still hoding it down
             | (b^.kbState) .&. 1 == 0 -> return True -- still up (this should not happen)
             | otherwise -> return False
