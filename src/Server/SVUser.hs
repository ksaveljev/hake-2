{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
module Server.SVUser where

import Control.Lens ((.=), preuse, ix, use, (^.), zoom, (-=), (+=), (&), (.~))
import Control.Monad (unless, when, liftM, void)
import Data.Bits ((.&.))
import Data.Maybe (fromJust)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V

import Quake
import QuakeState
import CVarVariables
import QCommon.XCommandT
import Server.UCmdT
import qualified Constants
import {-# SOURCE #-} qualified Game.Cmd as Cmd
import qualified Game.Info as Info
import qualified Game.PlayerClient as PlayerClient
import qualified QCommon.CBuf as CBuf
import {-# SOURCE #-} qualified QCommon.CVar as CVar
import {-# SOURCE #-} qualified QCommon.FS as FS
import qualified QCommon.MSG as MSG
import qualified QCommon.Com as Com
import qualified Server.SVMain as SVMain
import qualified Util.Lib as Lib

maxStringCmds :: Int
maxStringCmds = 8

nextServer :: Quake ()
nextServer = do
    state <- use $ svGlobals.svServer.sState
    coopValue <- CVar.variableValue "coop"

    -- ZOID, ss_pic can be nextserver'd in coop mode
    -- can't nextserver while playing a normal game
    unless (state == Constants.ssGame || (state == Constants.ssPic && coopValue == 0)) $ do
      svGlobals.svServerStatic.ssSpawnCount += 1 -- make sure another doesn't sneak in
      v <- CVar.variableString "nextserver"

      if B.length v == 0
        then
          CBuf.addText "killserver\n"
        else do
          CBuf.addText v
          CBuf.addText "\n"

      void $ CVar.set "nextserver" ""

nullCmd :: UserCmdT
nullCmd = newUserCmdT

nullState :: EntityStateT
nullState = newEntityStateT Nothing

uCmds :: V.Vector UCmdT
uCmds =
    V.fromList [ UCmdT "new" newF
               , UCmdT "configstrings" configStringsF
               , UCmdT "baselines" baselinesF
               , UCmdT "begin" beginF
               , UCmdT "nextserver" nextServerF
               , UCmdT "disconnect" disconnectF

               -- issued by hand at client consoles
               , UCmdT "info" showServerInfoF
               , UCmdT "download" beginDownloadF
               , UCmdT "nextdl" nextDownloadF
               ]

executeClientMessage :: ClientReference -> Quake ()
executeClientMessage clientRef@(ClientReference clientIdx) = do
    svGlobals.svClient .= Just clientRef
    Just edictRef <- preuse $ svGlobals.svServerStatic.ssClients.ix clientIdx.cEdict
    svGlobals.svPlayer .= edictRef

    -- only allow one move command
    executeMessage False 0

  where executeMessage :: Bool -> Int -> Quake ()
        executeMessage moveIssued stringCmdCount = do
          nm <- use $ globals.netMessage

          if (nm^.sbReadCount) > (nm^.sbCurSize)
            then do
              Com.printf "SV_ReadClientMessage: bad read:\n"
              -- Com.Printf(Lib.hexDump(Globals.net_message.data, 32, false));
              SVMain.dropClient clientRef
            else do
              c <- liftM fromIntegral $ MSG.readByte (globals.netMessage)

              (done, moveIssued', stringCmdCount') <- execute c moveIssued stringCmdCount

              unless done $
                executeMessage moveIssued' stringCmdCount'

        execute :: Int -> Bool -> Int -> Quake (Bool, Bool, Int)
        execute c moveIssued stringCmdCount = do
          if c == -1
            then return (True, moveIssued, stringCmdCount)
            else do
              if | c == Constants.clcNop -> return (True, moveIssued, stringCmdCount) 

                 | c == Constants.clcUserInfo -> do
                     io (putStrLn "SVUser.executeClientMessage#executeMessage#clcUserInfo") >> undefined -- TODO

                 | c == Constants.clcMove -> do
                     if moveIssued
                       then return (True, moveIssued, stringCmdCount) -- someone is trying to cheat...
                       else do
                         Just client <- preuse $ svGlobals.svServerStatic.ssClients.ix clientIdx

                         checksumIndex <- use $ globals.netMessage.sbReadCount
                         checksum <- MSG.readByte (globals.netMessage)
                         lastFrame <- MSG.readLong (globals.netMessage)

                         when (lastFrame /= (client^.cLastFrame)) $ do
                           svGlobals.svServerStatic.ssClients.ix clientIdx.cLastFrame .= lastFrame
                           when (lastFrame > 0) $ do
                             realTime <- use $ svGlobals.svServerStatic.ssRealTime
                             let idx = lastFrame .&. (Constants.latencyCounts - 1)
                             svGlobals.svServerStatic.ssClients.ix clientIdx.cFrameLatency.ix idx .= realTime - (((client^.cFrames) V.! (lastFrame .&. Constants.updateMask))^.cfSentTime)

                         oldest <- MSG.readDeltaUserCmd (globals.netMessage) nullCmd
                         oldcmd <- MSG.readDeltaUserCmd (globals.netMessage) oldest
                         newcmd <- MSG.readDeltaUserCmd (globals.netMessage) oldcmd

                         if (client^.cState) /= Constants.csSpawned
                           then do
                             svGlobals.svServerStatic.ssClients.ix clientIdx.cLastFrame .= -1
                             return (False, True, stringCmdCount)
                           else do
                             -- if the checksum fails, ignore the rest of the packet
                             nm <- use $ globals.netMessage
                             calculatedChecksum <- Com.blockSequenceCRCByte (nm^.sbData) (checksumIndex + 1) ((nm^.sbReadCount) - checksumIndex - 1) (client^.cNetChan.ncIncomingSequence)

                             if fromIntegral (calculatedChecksum .&. 0xFF) /= checksum
                               then do
                                 Com.dprintf $ "Failed checksum for ..." -- TODO: add more info
                                 return (True, True, stringCmdCount)
                               else do
                                 pausedValue <- liftM (^.cvValue) svPausedCVar

                                 when (pausedValue == 0) $ do
                                   let netDrop = client^.cNetChan.ncDropped
                                   when (netDrop < 20) $ do
                                     netDrop' <- execCmd clientRef (client^.cLastCmd) netDrop

                                     when (netDrop' > 1) $
                                       clientThink clientRef oldest

                                     when (netDrop' > 0) $
                                       clientThink clientRef oldcmd

                                   clientThink clientRef newcmd

                                 svGlobals.svServerStatic.ssClients.ix clientIdx.cLastCmd .= newcmd

                                 return (False, True, stringCmdCount)

                 | c == Constants.clcStringCmd -> do
                     s <- MSG.readString (globals.netMessage)

                     -- malicious users may try using too many string commands
                     let stringCmdCount' = stringCmdCount + 1
                     when (stringCmdCount' < maxStringCmds) $
                       executeUserCommand s

                     Just state <- preuse $ svGlobals.svServerStatic.ssClients.ix clientIdx.cState

                     if state == Constants.csZombie
                       then return (True, moveIssued, stringCmdCount')
                       else return (False, moveIssued, stringCmdCount')

                 | otherwise -> do
                     io (putStrLn "SVUser.executeClientMessage#executeMessage") >> undefined -- TODO

        execCmd :: ClientReference -> UserCmdT -> Int -> Quake Int
        execCmd cr lastcmd netDrop
          | netDrop > 2 = do
              clientThink cr lastcmd
              execCmd cr lastcmd (netDrop - 1)
          | otherwise = return netDrop

clientThink :: ClientReference -> UserCmdT -> Quake ()
clientThink (ClientReference clientIdx) cmd = do
    svGlobals.svServerStatic.ssClients.ix clientIdx.cCommandMsec -= (fromIntegral (cmd^.ucMsec) .&. 0xFF)

    Just client <- preuse $ svGlobals.svServerStatic.ssClients.ix clientIdx
    enforceTimeValue <- liftM (^.cvValue) svEnforceTimeCVar
    
    if (client^.cCommandMsec) < 0 && enforceTimeValue /= 0
      then Com.dprintf ("commandMsec underflow from " `B.append` (client^.cName) `B.append` "\n")
      else PlayerClient.clientThink (fromJust $ client^.cEdict) cmd

executeUserCommand :: B.ByteString -> Quake ()
executeUserCommand str = do
    Com.dprintf $ "SV_ExecuteUserCommand:" `B.append` str `B.append` "\n"

    Cmd.tokenizeString str True

    Just (ClientReference clientIdx) <- use $ svGlobals.svClient
    Just (Just edictRef) <- preuse $ svGlobals.svServerStatic.ssClients.ix clientIdx.cEdict

    svGlobals.svPlayer .= Just edictRef

    v0 <- Cmd.argv 0

    let foundCmd = V.find (\c -> (c^.ucName) == v0) uCmds

    case foundCmd of
      Just (UCmdT _ func) -> (func)^.xcCmd
      Nothing -> do
        state <- use $ svGlobals.svServer.sState
        when (state == Constants.ssGame) $
          Cmd.clientCommand edictRef

{-
- ================ SV_New_f
- 
- Sends the first message from the server to a connected client. This will
- be sent on the initial connection and upon each server load.
- ================
-}
newF :: XCommandT
newF =
  XCommandT "SVUser.newF" (do
    Just (ClientReference clientIdx) <- use $ svGlobals.svClient
    Just client <- preuse $ svGlobals.svServerStatic.ssClients.ix clientIdx
    state <- use $ svGlobals.svServer.sState

    Com.dprintf ("New() from " `B.append` (client^.cName) `B.append` "\n")

    if | (client^.cState) /= Constants.csConnected ->
           Com.printf "New not valid -- already spawned\n"
         -- demo servers just dump the file message
       | state == Constants.ssDemo ->
           beginDemoServer
       | otherwise -> do
           -- serverdata needs to go over for all types of servers
           -- to make sure the protocol is right, and to set the gamedir
           gameDir <- CVar.variableString "gamedir"

           -- send the serverdata
           
           MSG.writeByteI (svGlobals.svServerStatic.ssClients.ix clientIdx.cNetChan.ncMessage) Constants.svcServerData
           MSG.writeInt (svGlobals.svServerStatic.ssClients.ix clientIdx.cNetChan.ncMessage) Constants.protocolVersion

           spawnCount <- use $ svGlobals.svServerStatic.ssSpawnCount
           MSG.writeLong (svGlobals.svServerStatic.ssClients.ix clientIdx.cNetChan.ncMessage) spawnCount

           attractLoop <- use $ svGlobals.svServer.sAttractLoop
           MSG.writeByteI (svGlobals.svServerStatic.ssClients.ix clientIdx.cNetChan.ncMessage) (if attractLoop then 1 else 0)

           MSG.writeString (svGlobals.svServerStatic.ssClients.ix clientIdx.cNetChan.ncMessage) gameDir

           let playerNum = if state == Constants.ssCinematic || state == Constants.ssPic
                             then -1
                             else client^.cServerIndex

           MSG.writeShort (svGlobals.svServerStatic.ssClients.ix clientIdx.cNetChan.ncMessage) playerNum

           -- send full levelname
           Just levelName <- preuse $ svGlobals.svServer.sConfigStrings.ix (Constants.csName)
           MSG.writeString (svGlobals.svServerStatic.ssClients.ix clientIdx.cNetChan.ncMessage) levelName

           -- game server
           when (state == Constants.ssGame) $ do
             -- set up the entity for the client
             let edictRef = newEdictReference (playerNum + 1)
             modifyEdictT edictRef (\v -> v & eEntityState.esNumber .~ playerNum + 1)
             zoom (svGlobals.svServerStatic.ssClients.ix clientIdx) $ do
               cEdict .= Just edictRef
               cLastCmd .= newUserCmdT

             -- begin fetching configstrings
             MSG.writeByteI (svGlobals.svServerStatic.ssClients.ix clientIdx.cNetChan.ncMessage) Constants.svcStuffText
             MSG.writeString (svGlobals.svServerStatic.ssClients.ix clientIdx.cNetChan.ncMessage) ("cmd configstrings " `B.append` (BC.pack $ show spawnCount) `B.append` " 0\n") -- IMPROVE?
  )

configStringsF :: XCommandT
configStringsF =
  XCommandT "SVUser.configStringsF" (do
    Just clientRef@(ClientReference clientIdx) <- use $ svGlobals.svClient
    Just client <- preuse $ svGlobals.svServerStatic.ssClients.ix clientIdx
    Com.dprintf $ "Configstrings() from " `B.append` (client^.cName) `B.append` "\n"

    if (client^.cState) /= Constants.csConnected
      then Com.printf "configstrings not valid -- already spawned\n"
      else do
        -- handle the case of a level changing while a client was connecting
        v1 <- Cmd.argv 1
        spawnCount <- use $ svGlobals.svServerStatic.ssSpawnCount
        if Lib.atoi v1 /= spawnCount
          then do
            Com.printf "SV_Configstrings_f from different level\n"
            (newF)^.xcCmd
          else do
            v2 <- Cmd.argv 2
            let start = Lib.atoi v2

            -- write a packet full of data
            configStrings <- use $ svGlobals.svServer.sConfigStrings
            start' <- writeConfigStringsPacket configStrings clientRef start

            -- send next command
            if start' == Constants.maxConfigStrings
              then do
                MSG.writeByteI (svGlobals.svServerStatic.ssClients.ix clientIdx.cNetChan.ncMessage) Constants.svcStuffText
                MSG.writeString (svGlobals.svServerStatic.ssClients.ix clientIdx.cNetChan.ncMessage) ("cmd baselines " `B.append` BC.pack (show spawnCount) `B.append` " 0\n");
              else do
                MSG.writeByteI (svGlobals.svServerStatic.ssClients.ix clientIdx.cNetChan.ncMessage) Constants.svcStuffText
                MSG.writeString (svGlobals.svServerStatic.ssClients.ix clientIdx.cNetChan.ncMessage) ("cmd configstrings " `B.append` BC.pack (show spawnCount) `B.append` " " `B.append` BC.pack (show start') `B.append` "\n") -- IMPROVE?
  )

  where writeConfigStringsPacket :: V.Vector B.ByteString -> ClientReference -> Int -> Quake Int
        writeConfigStringsPacket configStrings clientRef@(ClientReference clientIdx) start = do
          Just curSize <- preuse $ svGlobals.svServerStatic.ssClients.ix clientIdx.cNetChan.ncMessage.sbCurSize
          if curSize < Constants.maxMsgLen `div` 2 && start < Constants.maxConfigStrings
            then do
              let cs = configStrings V.! start
              when (B.length cs /= 0) $ do
                MSG.writeByteI (svGlobals.svServerStatic.ssClients.ix clientIdx.cNetChan.ncMessage) Constants.svcConfigString
                MSG.writeShort (svGlobals.svServerStatic.ssClients.ix clientIdx.cNetChan.ncMessage) start
                MSG.writeString (svGlobals.svServerStatic.ssClients.ix clientIdx.cNetChan.ncMessage) cs
              writeConfigStringsPacket configStrings clientRef (start + 1)
            else
              return start

baselinesF :: XCommandT
baselinesF =
  XCommandT "SVUser.baselinesF" (do
    Just clientRef@(ClientReference clientIdx) <- use $ svGlobals.svClient
    Just client <- preuse $ svGlobals.svServerStatic.ssClients.ix clientIdx

    Com.dprintf $ "Baselines() from " `B.append` (client^.cName) `B.append` "\n"

    if (client^.cState) /= Constants.csConnected
      then
        Com.printf "baselines not valid -- already spawned\n"
      else do
        -- handle the case of a level changing while a client was connecting
        spawnCount <- use $ svGlobals.svServerStatic.ssSpawnCount
        v1 <- Cmd.argv 1
        if Lib.atoi v1 /= spawnCount
          then do
            Com.printf "SV_Baselines_f from different level\n"
            (newF)^.xcCmd
          else do
            v2 <- Cmd.argv 2
            let start = Lib.atoi v2

            -- write a packet full of data
            baselines <- use $ svGlobals.svServer.sBaselines
            start' <- writeBaselinePacket baselines clientRef start

            -- send next command
            if start' == Constants.maxEdicts
              then do
                MSG.writeByteI (svGlobals.svServerStatic.ssClients.ix clientIdx.cNetChan.ncMessage) Constants.svcStuffText
                MSG.writeString (svGlobals.svServerStatic.ssClients.ix clientIdx.cNetChan.ncMessage) ("precache " `B.append` BC.pack (show spawnCount) `B.append` "\n") -- IMPROVE?
              else do
                MSG.writeByteI (svGlobals.svServerStatic.ssClients.ix clientIdx.cNetChan.ncMessage) Constants.svcStuffText
                MSG.writeString (svGlobals.svServerStatic.ssClients.ix clientIdx.cNetChan.ncMessage) ("cmd baselines " `B.append` BC.pack (show spawnCount) `B.append` " " `B.append` BC.pack (show start') `B.append` "\n") -- IMPROVE?
  )

  where writeBaselinePacket :: V.Vector EntityStateT -> ClientReference -> Int -> Quake Int
        writeBaselinePacket baselines clientRef@(ClientReference clientIdx) start = do
          Just curSize <- preuse $ svGlobals.svServerStatic.ssClients.ix clientIdx.cNetChan.ncMessage.sbCurSize
          if curSize < Constants.maxMsgLen `div` 2 && start < Constants.maxEdicts
            then do
              let base = baselines V.! start
              when ((base^.esModelIndex) /= 0 || (base^.esSound) /= 0 || (base^.esEffects) /= 0) $ do
                MSG.writeByteI (svGlobals.svServerStatic.ssClients.ix clientIdx.cNetChan.ncMessage) Constants.svcSpawnBaseline
                MSG.writeDeltaEntity nullState base (svGlobals.svServerStatic.ssClients.ix clientIdx.cNetChan.ncMessage) True True
              writeBaselinePacket baselines clientRef (start + 1)
            else
              return start

beginF :: XCommandT
beginF =
  XCommandT "SVUser.beginF" (do
    Just (ClientReference clientIdx) <- use $ svGlobals.svClient
    Just client <- preuse $ svGlobals.svServerStatic.ssClients.ix clientIdx
    Com.dprintf $ "Begin() from " `B.append` (client^.cName) `B.append` "\n"

    -- handle the case of a level changing while a client was connecting
    v1 <- Cmd.argv 1
    spawnCount <- use $ svGlobals.svServerStatic.ssSpawnCount

    if Lib.atoi v1 /= spawnCount
      then do
        Com.printf "SV_Begin_f from different level\n"
        (newF)^.xcCmd
      else do
        svGlobals.svServerStatic.ssClients.ix clientIdx.cState .= Constants.csSpawned

        -- call the game begin function
        Just edictRef <- use $ svGlobals.svPlayer
        PlayerClient.clientBegin edictRef

        CBuf.insertFromDefer
  )

{-
- ================== SV_Nextserver_f ==================
- 
- A cinematic has completed or been aborted by a client, so move to the
- next server
-}
nextServerF :: XCommandT
nextServerF =
  XCommandT "SVUser.nextServerF" (do
    c <- Cmd.argv 1
    spawnCount <- use $ svGlobals.svServerStatic.ssSpawnCount
    Just (ClientReference clientIdx) <- use $ svGlobals.svClient
    Just name <- preuse $ svGlobals.svServerStatic.ssClients.ix clientIdx.cName

    if Lib.atoi c /= spawnCount
      then
        Com.dprintf ("Nextserver() from wrong level, from " `B.append` name `B.append` "\n")
        -- leftover from last server
      else do
        Com.dprintf ("Nextserver() from " `B.append` name `B.append` "\n")
        nextServer
  )

{-
- ================= SV_Disconnect_f =================
- 
- The client is going to disconnect, so remove the connection immediately
-
-}
disconnectF :: XCommandT
disconnectF =
  XCommandT "SVUser.disconnectF" (do
    Just client <- use $ svGlobals.svClient
    SVMain.dropClient client
  )

{-
- ================== SV_ShowServerinfo_f ==================
- 
- Dumps the serverinfo info string
-}
showServerInfoF :: XCommandT
showServerInfoF = XCommandT "SVUser.showServerInfoF" (CVar.serverInfo >>= Info.print)

beginDownloadF :: XCommandT
beginDownloadF =
  XCommandT "SVUser.beginDownloadF" (do
    io (putStrLn "SVUser.beginDownloadF") >> undefined -- TODO
  )

nextDownloadF :: XCommandT
nextDownloadF =
  XCommandT "SVUser.nextDownloadF" (do
    io (putStrLn "SVUser.nextDownloadF") >> undefined -- TODO
  )

beginDemoServer :: Quake ()
beginDemoServer = do
    name <- use $ svGlobals.svServer.sName
    let name' = "demos/" `B.append` name

    demoFile <- FS.fOpenFile name'

    case demoFile of
      Nothing -> Com.comError Constants.errDrop ("Couldn't open " `B.append` name' `B.append` "\n")
      Just _ -> svGlobals.svServer.sDemoFile .= demoFile
