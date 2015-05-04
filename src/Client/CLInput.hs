{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Client.CLInput where

import Control.Lens ((^.), use, ix, (.=), preuse, Lens', (%=), (-=), (+=), (*=), zoom)
import Control.Monad (void, unless, when, liftM)
import Data.Bits ((.&.), xor, (.|.), complement)
import Linear (_x, _y, _z)
import qualified Data.ByteString as B
import qualified Data.Vector as V

import Quake
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
upDown = io (putStrLn "CLInput.upDown") >> undefined -- TODO

upUp :: XCommandT
upUp = io (putStrLn "CLInput.upUp") >> undefined -- TODO

downDown :: XCommandT
downDown = io (putStrLn "CLInput.downDown") >> undefined -- TODO

downUp :: XCommandT
downUp = io (putStrLn "CLInput.downUp") >> undefined -- TODO

leftDown :: XCommandT
leftDown = io (putStrLn "CLInput.leftDown") >> undefined -- TODO

leftUp :: XCommandT
leftUp = io (putStrLn "CLInput.leftUp") >> undefined -- TODO

rightDown :: XCommandT
rightDown = io (putStrLn "CLInput.rightDown") >> undefined -- TODO

rightUp :: XCommandT
rightUp = io (putStrLn "CLInput.rightUp") >> undefined -- TODO

forwardDown :: XCommandT
forwardDown = io (putStrLn "CLInput.forwardDown") >> undefined -- TODO

forwardUp :: XCommandT
forwardUp = io (putStrLn "CLInput.forwardUp") >> undefined -- TODO

backDown :: XCommandT
backDown = io (putStrLn "CLInput.backDown") >> undefined -- TODO

backUp :: XCommandT
backUp = io (putStrLn "CLInput.backUp") >> undefined -- TODO

lookUpDown :: XCommandT
lookUpDown = io (putStrLn "CLInput.lookUpDown") >> undefined -- TODO

lookUpUp :: XCommandT
lookUpUp = io (putStrLn "CLInput.lookUpUp") >> undefined -- TODO

lookDownDown :: XCommandT
lookDownDown = io (putStrLn "CLInput.lookDownDown") >> undefined -- TODO

lookDownUp :: XCommandT
lookDownUp = io (putStrLn "CLInput.lookDownUp") >> undefined -- TODO

strafeDown :: XCommandT
strafeDown = io (putStrLn "CLInput.strafeDown") >> undefined -- TODO

strafeUp :: XCommandT
strafeUp = io (putStrLn "CLInput.strafeUp") >> undefined -- TODO

moveLeftDown :: XCommandT
moveLeftDown = io (putStrLn "CLInput.moveLeftDown") >> undefined -- TODO

moveLeftUp :: XCommandT
moveLeftUp = io (putStrLn "CLInput.moveLeftUp") >> undefined -- TODO

moveRightDown :: XCommandT
moveRightDown = io (putStrLn "CLInput.moveRightDown") >> undefined -- TODO

moveRightUp :: XCommandT
moveRightUp = io (putStrLn "CLInput.moveRightUp") >> undefined -- TODO

speedDown :: XCommandT
speedDown = io (putStrLn "CLInput.speedDown") >> undefined -- TODO

speedUp :: XCommandT
speedUp = io (putStrLn "CLInput.speedUp") >> undefined -- TODO

attackDown :: XCommandT
attackDown = io (putStrLn "CLInput.attackDown") >> undefined -- TODO

attackUp :: XCommandT
attackUp = io (putStrLn "CLInput.attackUp") >> undefined -- TODO

useDown :: XCommandT
useDown = io (putStrLn "CLInput.useDown") >> undefined -- TODO

useUp :: XCommandT
useUp = io (putStrLn "CLInput.useUp") >> undefined -- TODO

impulse :: XCommandT
impulse = io (putStrLn "CLInput.impulse") >> undefined -- TODO

kLookDown :: XCommandT
kLookDown = io (putStrLn "CLInput.kLookDown") >> undefined -- TODO

kLookUp :: XCommandT
kLookUp = io (putStrLn "CLInput.kLookUp") >> undefined -- TODO

sendCmd :: Quake ()
sendCmd = do
    -- build a command even if not connected

    -- save this command off for prediction
    cmdRef <- saveCommandForPrediction

    state <- use $ globals.cls.csState

    unless (state == Constants.caDisconnected || state == Constants.caConnecting) $ do
      curSize <- use $ globals.cls.csNetChan.ncMessage.sbCurSize
      lastSent <- use $ globals.cls.csNetChan.ncLastSent
      curTime <- use $ globals.curtime
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
    let p = Math3D.shortToAngle angle
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
