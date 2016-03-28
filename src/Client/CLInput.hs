{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
module Client.CLInput
  ( initializeInput
  , sendCmd
  ) where

import           Client.CLShared as CL
import           Client.ClientStateT
import           Client.ClientStaticT
import           Client.FrameT
import           Client.KButtonT
import qualified Client.SCR as SCR
import qualified Constants
import qualified Game.Cmd as Cmd
import           Game.CVarT
import           Game.PlayerStateT
import           Game.PMoveStateT
import           Game.UserCmdT
import qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import           QCommon.CVarVariables
import qualified QCommon.MSG as MSG
import qualified QCommon.NetChannel as NetChannel
import           QCommon.NetChanT
import           QCommon.SizeBufT
import qualified QCommon.SZ as SZ
import           QuakeRef
import           QuakeState
import qualified Sys.IN as IN
import qualified Sys.Timer as Timer
import           Types
import qualified Util.Lib as Lib
import qualified Util.Math3D as Math3D

import           Control.Lens (Lens', use, ix, (^.), (.=), (%=), (&), (.~), (+~), (-~), (*~), (%~))
import           Control.Monad (void, when)
import           Data.Bits (complement, xor, (.&.), (.|.))
import qualified Data.ByteString as B
import           Data.Int (Int64)
import           Linear (_x, _y)

initialCommands :: [(B.ByteString, Maybe XCommandT)]
initialCommands =
  [ ("centerview", Just IN.centerView)
  , ("+moveup", Just upDown)
  , ("-moveup", Just upUp)
  , ("+movedown", Just downDown)
  , ("-movedown", Just downUp)
  , ("+left", Just leftDown)
  , ("-left", Just leftUp)
  , ("+right", Just rightDown)
  , ("-right", Just rightUp)
  , ("+forward", Just forwardDown)
  , ("-forward", Just forwardUp)
  , ("+back", Just backDown)
  , ("-back", Just backUp)
  , ("+lookup", Just lookUpDown)
  , ("-lookup", Just lookUpUp)
  , ("+lookdown", Just lookDownDown)
  , ("-lookdown", Just lookDownUp)
  , ("+strafe", Just strafeDown)
  , ("-strafe", Just strafeUp)
  , ("+moveleft", Just moveLeftDown)
  , ("-moveleft", Just moveLeftUp)
  , ("+moveright", Just moveRightDown)
  , ("-moveright", Just moveRightUp)
  , ("+speed", Just speedDown)
  , ("-speed", Just speedUp)
  , ("+attack", Just attackDown)
  , ("-attack", Just attackUp)
  , ("+use", Just useDown)
  , ("-use", Just useUp)
  , ("impulse", Just impulse)
  , ("+klook", Just kLookDown)
  , ("-klook", Just kLookUp)
  ]

nullcmd :: UserCmdT
nullcmd = newUserCmdT

initializeInput :: Quake ()
initializeInput =
  do Cmd.addInitialCommands initialCommands
     void (CVar.get "cl_nodelta" "0" 0)

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
impulse = XCommandT "CLInput.impulse" $
  do arg <- Cmd.argv 1
     clientGlobals.cgInImpulse .= Lib.atoi arg

kLookDown :: XCommandT
kLookDown = XCommandT "CLInput.kLookDown" (inputKeyDown (clientGlobals.cgInKLook))

kLookUp :: XCommandT
kLookUp = XCommandT "CLInput.kLookUp" (inputKeyUp (clientGlobals.cgInKLook))

inputKeyDown :: Lens' QuakeState KButtonT -> Quake ()
inputKeyDown = error "CLInput.inputKeyDown" -- TODO

inputKeyUp :: Lens' QuakeState KButtonT -> Quake ()
inputKeyUp = error "CLInput.inputKeyUp" -- TODO

sendCmd :: Quake ()
sendCmd =
  do cmdRef <- saveCommandForPrediction =<< use (globals.gCls)
     doSendCmd cmdRef =<< use (globals.gCls.csState)

doSendCmd :: Ref UserCmdT -> Int -> Quake ()
doSendCmd cmdRef state
  | state `elem` [Constants.caDisconnected, Constants.caConnecting] = return ()
  | state == Constants.caConnected =
      do curSize <- use (globals.gCls.csNetChan.ncMessage.sbCurSize)
         curTime <- Timer.getCurTime
         lastSent <- use (globals.gCls.csNetChan.ncLastSent)
         when (curSize /= 0 || (curTime - lastSent) > 1000) $
           NetChannel.transmit (globals.gCls.csNetChan) 0 B.empty
  | otherwise =
      do sendUserUpdate =<< use (globals.gUserInfoModified)
         SZ.initialize (clientGlobals.cgBuf) B.empty 128
         checkSkipCinematics
         MSG.writeByteI (clientGlobals.cgBuf) (fromIntegral Constants.clcMove)
         checksumIndex <- use (clientGlobals.cgBuf.sbCurSize)
         MSG.writeByteI (clientGlobals.cgBuf) 0
         setCompressionInfo
         writeCommandsToBuf =<< use (globals.gCls.csNetChan.ncOutgoingSequence)
         calculateChecksum checksumIndex
         deliverMessage
  where checkSkipCinematics =
          do shouldSkip <- shouldSkipCinematic cmdRef
             when shouldSkip SCR.finishCinematic

sendUserUpdate :: Bool -> Quake ()
sendUserUpdate False = return ()
sendUserUpdate True =
  do CL.fixUpGender
     globals.gUserInfoModified .= False
     MSG.writeByteI (globals.gCls.csNetChan.ncMessage) (fromIntegral Constants.clcUserInfo)
     MSG.writeString (globals.gCls.csNetChan.ncMessage) =<< CVar.userInfo

shouldSkipCinematic :: Ref UserCmdT -> Quake Bool
shouldSkipCinematic cmdRef =
  do cmd <- readRef cmdRef
     realTime <- use (globals.gCls.csRealTime)
     cl <- use (globals.gCl)
     return ((cmd^.ucButtons) /= 0 && (cl^.csCinematicTime) > 0 && not (cl^.csAttractLoop) && realTime - (cl^.csCinematicTime) > 1000)

setCompressionInfo :: Quake ()
setCompressionInfo =
  do noDelta <- clNoDeltaCVar
     validFrame <- use (globals.gCl.csFrame.fValid)
     demoWaiting <- use (globals.gCls.csDemoWaiting)
     doSetCompressionInfo noDelta validFrame demoWaiting
  where doSetCompressionInfo noDelta validFrame demoWaiting
          | (noDelta^.cvValue) /= 0 || not validFrame || demoWaiting =
              MSG.writeLong (clientGlobals.cgBuf) (-1)
          | otherwise =
              MSG.writeLong (clientGlobals.cgBuf) =<< use (globals.gCl.csFrame.fServerFrame)

writeCommandsToBuf :: Int -> Quake ()
writeCommandsToBuf outgoingSequence =
  do cmdI <- readRef (Ref i)
     cmdJ <- readRef (Ref j)
     cmdK <- readRef (Ref k)
     MSG.writeDeltaUserCmd (clientGlobals.cgBuf) nullcmd cmdI
     MSG.writeDeltaUserCmd (clientGlobals.cgBuf) cmdI cmdJ
     MSG.writeDeltaUserCmd (clientGlobals.cgBuf) cmdJ cmdK
  where i = (outgoingSequence - 2) .&. (Constants.cmdBackup - 1)
        j = (outgoingSequence - 1) .&. (Constants.cmdBackup - 1)
        k = outgoingSequence .&. (Constants.cmdBackup - 1)

deliverMessage :: Quake ()
deliverMessage =
  do buf <- use (clientGlobals.cgBuf)
     NetChannel.transmit (globals.gCls.csNetChan) (buf^.sbCurSize) (buf^.sbData)

calculateChecksum :: Int -> Quake ()
calculateChecksum checksumIndex =
  do buf <- use (clientGlobals.cgBuf)
     outgoingSequence <- use (globals.gCls.csNetChan.ncOutgoingSequence)
     crcByte <- Com.blockSequenceCRCByte (buf^.sbData) (checksumIndex + 1) ((buf^.sbCurSize) - checksumIndex - 1) outgoingSequence
     clientGlobals.cgBuf.sbData .= ((B.take checksumIndex (buf^.sbData)) `B.snoc` crcByte) `B.append` (B.drop (checksumIndex + 1) (buf^.sbData)) -- IMPROVE?

saveCommandForPrediction :: ClientStaticT -> Quake (Ref UserCmdT)
saveCommandForPrediction cls =
  do globals.gCl.csCmdTime.ix idx .= (cls^.csRealTime)
     createCmd cmdRef
     cmd <- readRef cmdRef
     globals.gCl.csCmd .= cmd
     return cmdRef
  where idx = (cls^.csNetChan.ncOutgoingSequence) .&. (Constants.cmdBackup - 1)
        cmdRef = Ref idx

createCmd :: Ref UserCmdT -> Quake ()
createCmd cmdRef =
  do sysFrameTime <- use (globals.gSysFrameTime)
     oldSysFrameTime <- use (clientGlobals.cgOldSysFrameTime)
     clientGlobals.cgFrameMsec .= fromIntegral (calcFrameMsec sysFrameTime oldSysFrameTime)
     baseMove cmdRef
     IN.move cmdRef
     finishMove cmdRef
     updateOldSysFrameTime

calcFrameMsec :: Int -> Int64 -> Int64
calcFrameMsec sysFrameTime oldSysFrameTime
  | diff < 1 = 1
  | diff > 200 = 200
  | otherwise = diff
  where diff = fromIntegral sysFrameTime - oldSysFrameTime

updateOldSysFrameTime :: Quake ()
updateOldSysFrameTime =
  do sysFrameTime <- use (globals.gSysFrameTime)
     clientGlobals.cgOldSysFrameTime .= fromIntegral sysFrameTime

baseMove :: Ref UserCmdT -> Quake ()
baseMove cmdRef =
  do adjustAngles
     resetCmdRef
     inStrafe <- use (clientGlobals.cgInStrafe)
     inKLook <- use (clientGlobals.cgInKLook)
     inSpeed <- use (clientGlobals.cgInSpeed)
     sideSpeed <- clSideSpeedCVar
     checkInStrafe inStrafe sideSpeed
     updateSideMove sideSpeed
     updateUpMove =<< clUpSpeedCVar
     checkForwardMove inKLook =<< clForwardSpeedCVar
     adjustRunningSpeed inSpeed =<< clRunCVar
  where resetCmdRef =
          do angles <- use (globals.gCl.csViewAngles)
             writeRef cmdRef (newUserCmdT & ucAngles .~ fmap truncate angles)
        checkInStrafe inStrafe sideSpeed
          | (inStrafe^.kbState) .&. 1 /= 0 =
              do right <- keyState (clientGlobals.cgInRight)
                 left <- keyState (clientGlobals.cgInLeft)
                 modifyRef cmdRef (\v -> v & ucSideMove +~ truncate ((sideSpeed^.cvValue) * right)
                                           & ucSideMove -~ truncate ((sideSpeed^.cvValue) * left))
          | otherwise = return ()
        updateSideMove sideSpeed =
          do moveRight <- keyState (clientGlobals.cgInMoveRight)
             moveLeft <- keyState (clientGlobals.cgInMoveLeft)
             modifyRef cmdRef (\v -> v & ucSideMove +~ truncate ((sideSpeed^.cvValue) * moveRight)
                                       & ucSideMove -~ truncate ((sideSpeed^.cvValue) * moveLeft))
        updateUpMove upSpeed =
          do up <- keyState (clientGlobals.cgInUp)
             down <- keyState (clientGlobals.cgInDown)
             modifyRef cmdRef (\v -> v & ucUpMove +~ truncate ((upSpeed^.cvValue) * up)
                                       & ucUpMove -~ truncate ((upSpeed^.cvValue) * down))
        checkForwardMove inKLook forwardSpeed
          | (inKLook^.kbState) .&. 1 == 0 =
              do forward <- keyState (clientGlobals.cgInForward)
                 back <- keyState (clientGlobals.cgInBack)
                 modifyRef cmdRef (\v -> v & ucForwardMove +~ truncate ((forwardSpeed^.cvValue) * forward)
                                           & ucForwardMove -~ truncate ((forwardSpeed^.cvValue) * back))
          | otherwise = return ()
        adjustRunningSpeed inSpeed run
          | ((inSpeed^.kbState) .&. 1) `xor` (truncate (run^.cvValue)) /= 0 =
              modifyRef cmdRef (\v -> v & ucForwardMove *~ 2
                                        & ucSideMove *~ 2
                                        & ucUpMove *~ 2)
          | otherwise = return ()

finishMove :: Ref UserCmdT -> Quake ()
finishMove cmdRef =
  do checkInAttack cmdRef
     checkInUse cmdRef
     checkKeyDown cmdRef
     calcMs cmdRef =<< use (globals.gCls.csFrameTime)
     clampPitch
     updateViewAngles =<< use (globals.gCl.csViewAngles)
     updateImpulse =<< use (clientGlobals.cgInImpulse)
     updateLightLevel =<< clLightLevelCVar
  where updateViewAngles viewAngles =
          modifyRef cmdRef (\v -> v & ucAngles .~ fmap Math3D.angleToShort viewAngles)
        updateImpulse inImpulse =
          do modifyRef cmdRef (\v -> v & ucImpulse .~ fromIntegral inImpulse)
             clientGlobals.cgInImpulse .= 0
        updateLightLevel lightLevel =
          modifyRef cmdRef (\v -> v & ucLightLevel .~ truncate (lightLevel^.cvValue))

checkInAttack :: Ref UserCmdT -> Quake ()
checkInAttack cmdRef =
  do inAttack <- use (clientGlobals.cgInAttack)
     when ((inAttack^.kbState) .&. 3 /= 0) $
       modifyRef cmdRef (\v -> v & ucButtons %~ (.|. (fromIntegral Constants.buttonAttack)))
     clientGlobals.cgInAttack.kbState %= (.&. (complement 2))

checkInUse :: Ref UserCmdT -> Quake ()
checkInUse cmdRef =
  do inUse <- use (clientGlobals.cgInUse)
     when ((inUse^.kbState) .&. 3 /= 0) $
       modifyRef cmdRef (\v -> v & ucButtons %~ (.|. (fromIntegral Constants.buttonUse)))
     clientGlobals.cgInUse.kbState %= (.&. (complement 2))

checkKeyDown :: Ref UserCmdT -> Quake ()
checkKeyDown cmdRef =
  do anyKeyDown <- use (keyGlobals.kgAnyKeyDown)
     keyDest <- use (globals.gCls.csKeyDest)
     when (anyKeyDown /= 0 && keyDest == Constants.keyGame) $
       modifyRef cmdRef (\v -> v & ucButtons %~ (.|. (fromIntegral Constants.buttonAny)))

calcMs :: Ref UserCmdT -> Float -> Quake ()
calcMs cmdRef frameTime =
  modifyRef cmdRef (\v -> v & ucMsec .~ fromIntegral ms')
  where ms = truncate (frameTime * 1000) :: Int
        ms' | ms > 250 = 100
            | otherwise = ms

keyState :: Lens' QuakeState KButtonT -> Quake Float
keyState keyLens =
  do key <- use keyLens
     keyLens %= (\v -> v & kbState %~ (.&. 1)
                         & kbMsec .~ 0)
     msec <- calcMsec key
     calcResult msec <$> use (clientGlobals.cgFrameMsec)
  where calcMsec key
          | (key^.kbState) /= 0 =
              do sysFrameTime <- fmap fromIntegral (use (globals.gSysFrameTime))
                 keyLens.kbDownTime .= sysFrameTime
                 return ((key^.kbMsec) + sysFrameTime - (key^.kbDownTime))
          | otherwise = return (key^.kbMsec)
        calcResult msec frameMsec
          | val < 0 = 0
          | val > 1 = 1
          | otherwise = val
          where val = fromIntegral msec / fromIntegral frameMsec

adjustAngles :: Quake ()
adjustAngles =
  do frameTime <- use (globals.gCls.csFrameTime)
     pitchSpeed <- clPitchSpeedCVar
     speed <- calcSpeed frameTime =<< use (clientGlobals.cgInSpeed)
     checkStrafe speed =<< use (clientGlobals.cgInStrafe)
     checkKLook speed pitchSpeed =<< use (clientGlobals.cgInKLook)
     up <- keyState (clientGlobals.cgInLookUp)
     down <- keyState (clientGlobals.cgInLookDown)
     globals.gCl.csViewAngles %= (\v -> v & _x -~ speed * (pitchSpeed^.cvValue) * up
                                          & _x +~ speed * (pitchSpeed^.cvValue) * down)
  where calcSpeed frameTime inSpeed
          | (inSpeed^.kbState) .&. 1 /= 0 =
              fmap ((* frameTime) . (^.cvValue)) clAngleSpeedkeyCVar
          | otherwise = return frameTime
        checkStrafe speed inStrafe
          | (inStrafe^.kbState) .&. 1 == 0 =
              do yawSpeed <- clYawSpeedCVar
                 right <- keyState (clientGlobals.cgInRight)
                 left <- keyState (clientGlobals.cgInLeft)
                 globals.gCl.csViewAngles %= (\v -> v & _y -~ speed * (yawSpeed^.cvValue) * right
                                                      & _y +~ speed * (yawSpeed^.cvValue) * left)
          | otherwise = return ()
        checkKLook speed pitchSpeed inKLook
          | (inKLook^.kbState) .&. 1 /= 0 =
              do forward <- keyState (clientGlobals.cgInForward)
                 back <- keyState (clientGlobals.cgInBack)
                 globals.gCl.csViewAngles %= (\v -> v & _x -~ speed * (pitchSpeed^.cvValue) * forward
                                                      & _x +~ speed * (pitchSpeed^.cvValue) * back)
          | otherwise = return ()

clampPitch :: Quake ()
clampPitch =
  do pitch <- use (globals.gCl.csFrame.fPlayerState.psPMoveState.pmsDeltaAngles._x)
     doClampPitch (Math3D.shortToAngle (fromIntegral pitch))

doClampPitch :: Float -> Quake ()
doClampPitch p =
  do globals.gCl.csViewAngles._x %= (\v -> if v + pitch < (-360) then v + 360 else v)
     globals.gCl.csViewAngles._x %= (\v -> if v + pitch > 360 then v - 360 else v)
     globals.gCl.csViewAngles._x %= (\v -> if v + pitch > 89 then 89 - pitch else v)
     globals.gCl.csViewAngles._x %= (\v -> if v + pitch < (-89) then (-89 - pitch) else v)
  where pitch | p > 180 = p - 360
              | otherwise = p