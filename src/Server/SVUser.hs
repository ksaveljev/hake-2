module Server.SVUser
    ( executeClientMessage
    , nextServer
    ) where

import           Control.Lens          (Traversal', preuse, use, ix)
import           Control.Lens          ((^.), (.=), (+=), (&), (.~), (-~))
import           Control.Monad         (when, unless, void, (>=>))
import           Data.Bits             ((.&.))
import qualified Data.ByteString       as B
import qualified Data.Vector           as V

import qualified Constants
import qualified Game.Cmd              as Cmd
import           Game.CVarT
import           Game.EdictT
import           Game.EntityStateT
import qualified Game.Info             as Info
import qualified Game.PlayerClient     as PlayerClient
import           Game.UserCmdT
import qualified QCommon.CBuf          as CBuf
import qualified QCommon.Com           as Com
import qualified QCommon.CVar          as CVar
import           QCommon.CVarVariables
import qualified QCommon.FS            as FS
import qualified QCommon.MSG           as MSG
import           QCommon.NetChanT
import           QCommon.SizeBufT
import           QCommon.XCommandT
import           QuakeRef
import           QuakeState
import           Server.ClientFrameT
import           Server.ClientT
import           Server.ServerStaticT
import           Server.ServerT
import           Server.UCmdT
import           Types
import           Util.Binary           (encode)
import qualified Util.Lib              as Lib

import {-# SOURCE #-} qualified Server.SVMain as SVMain

maxStringCmds :: Int
maxStringCmds = 8

nullCmd :: UserCmdT
nullCmd = newUserCmdT

nullState :: EntityStateT
nullState = newEntityStateT Nothing

uCmds :: V.Vector UCmdT
uCmds = V.fromList
    [ UCmdT "new" newF
    , UCmdT "configstrings" configStringsF
    , UCmdT "baselines" baselinesF
    , UCmdT "begin" beginF
    , UCmdT "nextserver" nextServerF
    , UCmdT "disconnect" disconnectF
    , UCmdT "info" showServerInfoF
    , UCmdT "download" beginDownloadF
    , UCmdT "nextdl" nextDownloadF
    ]

executeClientMessage :: Ref ClientT -> Quake ()
executeClientMessage clientRef = do
    svGlobals.svClient .= Just clientRef
    client <- readRef clientRef
    svGlobals.svPlayer .= (client^.cEdict)
    executeMessage clientRef False 0 =<< use (globals.gNetMessage)

executeMessage :: Ref ClientT -> Bool -> Int -> SizeBufT -> Quake ()
executeMessage clientRef moveIssued stringCmdCount netMessage
    | (netMessage^.sbReadCount) > (netMessage^.sbCurSize) = do
        Com.printf "SV_ReadClientMessage: bad read:\n"
        -- Com.Printf(Lib.hexDump(Globals.net_message.data, 32, false)); -- TODO
        SVMain.dropClient clientRef
    | otherwise = do
        c <- fmap fromIntegral (MSG.readByte (globals.gNetMessage))
        (done, moveIssued', stringCmdCount') <- execute clientRef c moveIssued stringCmdCount
        unless done $
            executeMessage clientRef moveIssued' stringCmdCount' =<< use (globals.gNetMessage)

execute :: Ref ClientT -> Int -> Bool -> Int -> Quake (Bool, Bool, Int)
execute clientRef c moveIssued stringCmdCount
    | c == -1 = return (True, moveIssued, stringCmdCount)
    | c == Constants.clcNop = return (True, moveIssued, stringCmdCount)
    | c == Constants.clcUserInfo = error "SVUser.execute clcUserInfo" -- TODO
    | c == Constants.clcMove && moveIssued = return (True, moveIssued, stringCmdCount) -- someone is trying to cheat...
    | c == Constants.clcMove = do
        client <- readRef clientRef
        checksumIndex <- use (globals.gNetMessage.sbReadCount)
        checksum <- MSG.readByte (globals.gNetMessage)
        lastFrame <- MSG.readLong (globals.gNetMessage)
        checkFrameLatency client lastFrame
        oldest <- MSG.readDeltaUserCmd (globals.gNetMessage) nullCmd
        oldcmd <- MSG.readDeltaUserCmd (globals.gNetMessage) oldest
        newcmd <- MSG.readDeltaUserCmd (globals.gNetMessage) oldcmd
        executeMove clientRef client oldest oldcmd newcmd stringCmdCount checksumIndex checksum
    | c == Constants.clcStringCmd = do
        str <- MSG.readString (globals.gNetMessage)
        client <- readRef clientRef
        when (stringCmdCount + 1 < maxStringCmds) (executeUserCommand str)
        return ((client^.cState) == Constants.csZombie, moveIssued, stringCmdCount + 1)
    | otherwise = error "SVUser.execute otherwise" -- TODO
  where
    checkFrameLatency client lastFrame
        | lastFrame /= (client^.cLastFrame) = do
            modifyRef clientRef (\v -> v & cLastFrame .~ lastFrame)
            when (lastFrame > 0) $ do
                realTime <- use (svGlobals.svServerStatic.ssRealTime)
                let idx = lastFrame .&. (Constants.latencyCounts - 1)
                modifyRef clientRef (\v -> v & cFrameLatency.ix idx .~ realTime - (((client^.cFrames) V.! (lastFrame .&. Constants.updateMask))^.cfSentTime))
        | otherwise = return ()

executeMove :: Ref ClientT -> ClientT -> UserCmdT -> UserCmdT -> UserCmdT -> Int -> Int -> Int -> Quake (Bool, Bool, Int)
executeMove clientRef client oldest oldcmd newcmd stringCmdCount checksumIndex checksum
    | (client^.cState) /= Constants.csSpawned = do
        modifyRef clientRef (\v -> v & cLastFrame .~ (-1))
        return (False, True, stringCmdCount)
    | otherwise = do
        -- if the checksum fails, ignore the rest of the packet
        nm <- use (globals.gNetMessage)
        calculatedChecksum <- Com.blockSequenceCRCByte (nm^.sbData) (checksumIndex + 1) ((nm^.sbReadCount) - checksumIndex - 1) (client^.cNetChan.ncIncomingSequence)
        doExecuteMove calculatedChecksum
  where
    doExecuteMove calculatedChecksum
        | fromIntegral (calculatedChecksum .&. 0xFF) /= checksum = do
            Com.dprintf "Failed checksum for ..." -- TODO: add more info
            return (True, True, stringCmdCount)
        | otherwise = do
            paused <- fmap (^.cvValue) pausedCVar
            when (paused == 0) $ do
                when ((client^.cNetChan.ncDropped) < 20) $ do
                    netDrop <- execCmd (client^.cLastCmd) (client^.cNetChan.ncDropped)
                    when (netDrop > 1) $
                        clientThink clientRef oldest
                    when (netDrop > 0) $
                        clientThink clientRef oldcmd
                clientThink clientRef newcmd
            modifyRef clientRef (\v -> v & cLastCmd .~ newcmd)
            return (False, True, stringCmdCount)
    execCmd lastcmd netDrop
        | netDrop > 2 = do
            clientThink clientRef lastcmd
            execCmd lastcmd (netDrop - 1)
        | otherwise = return netDrop

executeUserCommand :: B.ByteString -> Quake ()
executeUserCommand str = do
    Com.dprintf (B.concat ["SV_ExecuteUserCommand:", str, "\n"])
    Cmd.tokenizeString str True
    clientRef <- use (svGlobals.svClient)
    maybe clientRefError proceedExecuteUserCommand clientRef
  where
    clientRefError = Com.fatalError "SVUser.executeUserCommand clientRef is Nothing"
    edictRefError = Com.fatalError "SVUser.executeUserCommand client^.cEdict is Nothing"
    proceedExecuteUserCommand clientRef = do
        client <- readRef clientRef
        maybe edictRefError doExecuteUserCommand (client^.cEdict)

doExecuteUserCommand :: Ref EdictT -> Quake ()
doExecuteUserCommand edictRef = do
    svGlobals.svPlayer .= Just edictRef
    arg <- Cmd.argv 0
    runCmd (V.find (\c -> (c^.ucName) == arg) uCmds)
  where
    runCmd (Just cmd) = runXCommandT (cmd^.ucFunc)
    runCmd Nothing = do
        state <- use (svGlobals.svServer.sState)
        when (state == Constants.ssGame) (Cmd.clientCommand edictRef)

newF :: XCommandT
newF = XCommandT "SVUser.newF" new

new :: Quake ()
new = do
    clientRef <- use (svGlobals.svClient)
    maybe clientRefError proceedNew clientRef
  where
    clientRefError = Com.fatalError "SVUser.newF clientRef is Nothing"
    proceedNew clientRef = do
        client <- readRef clientRef
        state <- use (svGlobals.svServer.sState)
        Com.dprintf (B.concat ["New() from ", client^.cName, "\n"])
        doNew clientRef client state

doNew :: Ref ClientT -> ClientT -> Int -> Quake ()
doNew clientRef@(Ref clientIdx) client state
    | (client^.cState) /= Constants.csConnected = Com.printf "New not valid -- already spawned\n"
    | state == Constants.ssDemo = beginDemoServer =<< use (svGlobals.svServer.sName)
    | otherwise = do
        gameDir <- CVar.variableString "gamedir"
        MSG.writeByteI messageLens Constants.svcServerData
        MSG.writeInt messageLens Constants.protocolVersion
        spawnCount <- use (svGlobals.svServerStatic.ssSpawnCount)
        MSG.writeLong messageLens spawnCount
        attractLoop <- use (svGlobals.svServer.sAttractLoop)
        MSG.writeByteI messageLens (if attractLoop then 1 else 0)
        MSG.writeString messageLens gameDir
        MSG.writeShort messageLens playerNum
        sendFullLevelName =<< preuse (svGlobals.svServer.sConfigStrings.ix Constants.csName)
        when (state == Constants.ssGame) $ do
            modifyRef (Ref (playerNum + 1)) (\v -> v & eEntityState.esNumber .~ playerNum + 1)
            modifyRef clientRef (\v -> v & cEdict .~ Just (Ref (playerNum + 1))
                                         & cLastCmd .~ newUserCmdT)
            MSG.writeByteI messageLens Constants.svcStuffText
            MSG.writeString messageLens (B.concat ["cmd configstrings ", encode spawnCount, " 0\n"])
  where
    playerNum | state `elem` [Constants.ssCinematic, Constants.ssPic] = -1
              | otherwise = client^.cServerIndex
    sendFullLevelName Nothing = Com.fatalError "SVUser.doNew configString is Nothing"
    sendFullLevelName (Just levelName) = MSG.writeString messageLens levelName
    messageLens :: Traversal' QuakeState SizeBufT
    messageLens = svGlobals.svServerStatic.ssClients.ix clientIdx.cNetChan.ncMessage

configStringsF :: XCommandT
configStringsF = XCommandT "SVUser.configStringsF" $ do
    clientRef <- use (svGlobals.svClient)
    maybe clientError doConfigStrings clientRef
  where
    clientError = Com.fatalError "SVUser.configStringsF clientRef is Nothing"
    doConfigStrings clientRef = do
        client <- readRef clientRef
        Com.dprintf (B.concat ["Configstrings() from ", client^.cName, "\n"])
        processConfigStrings clientRef client
    processConfigStrings clientRef client
        | (client^.cState) /= Constants.csConnected =
            Com.printf "configstrings not valid -- already spawned\n"
        | otherwise = do
            -- handle the case of a level changing while a client was connecting
            v1 <- Cmd.argv 1
            spawnCount <- use (svGlobals.svServerStatic.ssSpawnCount)
            proceedConfigStrings clientRef v1 spawnCount
    proceedConfigStrings clientRef@(Ref clientIdx) v1 spawnCount
        | Lib.atoi v1 /= spawnCount = do
            Com.printf "SV_Configstrings_f from different level\n"
            runXCommandT newF
        | otherwise = do
            v2 <- Cmd.argv 2
            -- write a packet full of data
            configStrings <- use (svGlobals.svServer.sConfigStrings)
            start <- writeConfigStringsPacket configStrings clientRef (Lib.atoi v2)
            -- send next command
            if start == Constants.maxConfigStrings
                then do
                    MSG.writeByteI (svGlobals.svServerStatic.ssClients.ix clientIdx.cNetChan.ncMessage) Constants.svcStuffText
                    MSG.writeString (svGlobals.svServerStatic.ssClients.ix clientIdx.cNetChan.ncMessage) (B.concat ["cmd baselines ", encode spawnCount, " 0\n"]);
                else do
                    MSG.writeByteI (svGlobals.svServerStatic.ssClients.ix clientIdx.cNetChan.ncMessage) Constants.svcStuffText
                    MSG.writeString (svGlobals.svServerStatic.ssClients.ix clientIdx.cNetChan.ncMessage) (B.concat ["cmd configstrings ", encode spawnCount, " ", encode start, "\n"])
    writeConfigStringsPacket configStrings clientRef@(Ref clientIdx) start = do
        curSize <- fmap (^.cNetChan.ncMessage.sbCurSize) (readRef clientRef)
        if curSize < Constants.maxMsgLen `div` 2 && start < Constants.maxConfigStrings
            then do
                when (B.length (configStrings V.! start) /= 0) $ do
                    MSG.writeByteI (svGlobals.svServerStatic.ssClients.ix clientIdx.cNetChan.ncMessage) Constants.svcConfigString
                    MSG.writeShort (svGlobals.svServerStatic.ssClients.ix clientIdx.cNetChan.ncMessage) start
                    MSG.writeString (svGlobals.svServerStatic.ssClients.ix clientIdx.cNetChan.ncMessage) (configStrings V.! start)
                writeConfigStringsPacket configStrings clientRef (start + 1)
            else
                return start

baselinesF :: XCommandT
baselinesF = XCommandT "SVUser.baselinesF" $ do
    clientRef <- use (svGlobals.svClient)
    maybe clientError doBaselines clientRef
  where
    clientError = Com.fatalError "SVUser.configStringsF clientRef is Nothing"
    doBaselines clientRef = do
        client <- readRef clientRef
        Com.dprintf (B.concat ["Baselines() from ", client^.cName, "\n"])
        processBaselines clientRef client
    processBaselines clientRef client
        | (client^.cState) /= Constants.csConnected =
            Com.printf "baselines not valid -- already spawned\n"
        | otherwise = do
            -- handle the case of a level changing while a client was connecting
            v1 <- Cmd.argv 1
            spawnCount <- use (svGlobals.svServerStatic.ssSpawnCount)
            proceedBaselines clientRef v1 spawnCount
    proceedBaselines clientRef@(Ref clientIdx) v1 spawnCount
        | Lib.atoi v1 /= spawnCount = do
            Com.printf "SV_Baselines_f from different level\n"
            runXCommandT newF
        | otherwise = do
            v2 <- Cmd.argv 2
            -- write a packet full of data
            baselines <- use (svGlobals.svServer.sBaselines)
            start <- writeBaselinePacket baselines clientRef (Lib.atoi v2)
            -- send next command
            if start == Constants.maxEdicts
                then do
                    MSG.writeByteI (svGlobals.svServerStatic.ssClients.ix clientIdx.cNetChan.ncMessage) Constants.svcStuffText
                    MSG.writeString (svGlobals.svServerStatic.ssClients.ix clientIdx.cNetChan.ncMessage) (B.concat ["precache ", encode spawnCount, "\n"])
                else do
                    MSG.writeByteI (svGlobals.svServerStatic.ssClients.ix clientIdx.cNetChan.ncMessage) Constants.svcStuffText
                    MSG.writeString (svGlobals.svServerStatic.ssClients.ix clientIdx.cNetChan.ncMessage) (B.concat ["cmd baselines ", encode spawnCount, " ", encode start, "\n"])
    writeBaselinePacket baselines clientRef@(Ref clientIdx) start = do
      curSize <- fmap (^.cNetChan.ncMessage.sbCurSize) (readRef clientRef)
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
beginF = XCommandT "SVUser.beginF" $ do
    clientRef <- use (svGlobals.svClient)
    maybe beginError proceedBeginF clientRef
  where
    beginError = Com.fatalError "SVUser.beginF clientRef is Nothing"
    proceedBeginF clientRef = do
        client <- readRef clientRef
        Com.dprintf (B.concat ["Begin() from ", client^.cName, "\n"])
        c <- Cmd.argv 1
        spawnCount <- use (svGlobals.svServerStatic.ssSpawnCount)
        checkSpawnCount clientRef c spawnCount
    checkSpawnCount clientRef c spawnCount
        | Lib.atoi c /= spawnCount = do
            Com.printf "SV_Begin_f from different level\n"
            runXCommandT newF
        | otherwise = do
            modifyRef clientRef (\v -> v & cState .~ Constants.csSpawned)
            -- call the game begin function
            edictRef <- use (svGlobals.svPlayer)
            maybe edictError beginGame edictRef
    edictError = Com.fatalError "SVUser.beginF edictRef is Nothing"
    beginGame edictRef = do
        PlayerClient.clientBegin edictRef
        CBuf.insertFromDefer

nextServerF :: XCommandT
nextServerF = XCommandT "SVUser.nextServerF" $ do
    c <- Cmd.argv 1
    spawnCount <- use (svGlobals.svServerStatic.ssSpawnCount)
    clientRef <- use (svGlobals.svClient)
    maybe nextServerError (proceedNextServerF c spawnCount) clientRef
  where
    nextServerError = Com.fatalError "SVUser.nextServerF clientRef is Nothing"
    proceedNextServerF c spawnCount clientRef = do
        client <- readRef clientRef
        checkSpawnCount c spawnCount (client^.cName)
    checkSpawnCount c spawnCount name
        | Lib.atoi c /= spawnCount =
            Com.dprintf (B.concat ["Nextserver() from wrong level, from ", name, "\n"])
        | otherwise = do
            Com.dprintf (B.concat ["Nextserver() from ", name, "\n"])
            nextServer

disconnectF :: XCommandT
disconnectF = XCommandT "SVUser.disconnectF" disconnect

disconnect :: Quake ()
disconnect = do
    clientRef <- use (svGlobals.svClient)
    maybe clientRefError SVMain.dropClient clientRef
  where
    clientRefError = Com.fatalError "SVUser.disconnect clientRef is Nothing"

showServerInfoF :: XCommandT
showServerInfoF = XCommandT "SVUser.showServerInfoF" (CVar.serverInfo >>= Info.printInfo)

beginDownloadF :: XCommandT
beginDownloadF = error "SVUser.beginDownloadF" -- TODO

nextDownloadF :: XCommandT
nextDownloadF = error "SVUser.nextDownloadF" -- TODO

beginDemoServer :: B.ByteString -> Quake ()
beginDemoServer name = (FS.fOpenFile >=> saveDemoHandle) fileName
  where
    fileName = "demos/" `B.append` name
    saveDemoHandle Nothing = Com.comError Constants.errDrop (B.concat ["Couldn't open ", fileName, "\n"])
    saveDemoHandle fileHandle = svGlobals.svServer.sDemoFile .= fileHandle

nextServer :: Quake ()
nextServer = do
    state <- use (svGlobals.svServer.sState)
    coopValue <- CVar.variableValue "coop"
    unless (state == Constants.ssGame || (state == Constants.ssPic && coopValue == 0)) $ do
        svGlobals.svServerStatic.ssSpawnCount += 1 -- make sure another doesn't sneak in
        v <- CVar.variableString "nextserver"
        addText v
        void (CVar.set "nextserver" B.empty)
  where
    addText v
        | B.null v = CBuf.addText "killserver\n"
        | otherwise = do
            CBuf.addText v
            CBuf.addText "\n"

clientThink :: Ref ClientT -> UserCmdT -> Quake ()
clientThink clientRef cmd = do
    modifyRef clientRef (\v -> v & cCommandMsec -~ (fromIntegral (cmd^.ucMsec) .&. 0xFF))
    client <- readRef clientRef
    enforceTime <- fmap (^.cvValue) svEnforceTimeCVar
    doClientThink client enforceTime
  where
    doClientThink client enforceTime
        | (client^.cCommandMsec) < 0 && enforceTime /= 0 =
            Com.dprintf ("commandMsec underflow from " `B.append` (client^.cName) `B.append` "\n")
        | otherwise = do
            maybe edictError (\edictRef -> PlayerClient.clientThink edictRef cmd) (client^.cEdict)
    edictError =
        Com.fatalError "SVUser.clientThink client^.cEdict is Nothing"
