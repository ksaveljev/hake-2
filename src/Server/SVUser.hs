module Server.SVUser
    ( executeClientMessage
    , nextServer
    ) where

import           Control.Lens         (Traversal', preuse, use, ix)
import           Control.Lens         ((^.), (.=), (&), (.~))
import           Control.Monad        (when, unless, (>=>))
import           Data.Bits            ((.&.))
import qualified Data.ByteString      as B
import qualified Data.Vector          as V

import qualified Constants
import qualified Game.Cmd             as Cmd
import           Game.EdictT
import           Game.EntityStateT
import qualified Game.Info            as Info
import qualified Game.PlayerClient    as PlayerClient
import           Game.UserCmdT
import qualified QCommon.CBuf         as CBuf
import qualified QCommon.Com          as Com
import qualified QCommon.CVar         as CVar
import qualified QCommon.FS           as FS
import qualified QCommon.MSG          as MSG
import           QCommon.NetChanT
import           QCommon.SizeBufT
import           QCommon.XCommandT
import           QuakeRef
import           QuakeState
import           Server.ClientFrameT
import           Server.ClientT
import           Server.ServerStaticT
import           Server.ServerT
import qualified Server.SVMainShared  as SVMain
import           Server.UCmdT
import           Types
import           Util.Binary          (encode)
import qualified Util.Lib             as Lib

maxStringCmds :: Int
maxStringCmds = 8

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
        lastFrame <- MSG.readByte (globals.gNetMessage)
        checkFrameLatency client lastFrame
        error "SVUser.execute" -- TODO
    | c == Constants.clcStringCmd = do
        str <- MSG.readString (globals.gNetMessage)
        client <- readRef clientRef
        when (stringCmdCount + 1 < maxStringCmds) (executeUserCommand str)
        return ((client^.cState) == Constants.csZombie, moveIssued, stringCmdCount + 1)
    | otherwise = error "SVUser.execute otherwise" -- TODO
  where
    checkFrameLatency client lastFrame = error "SVUser.execute#checkFrameLatency"

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
configStringsF = error "SVUser.configStringsF" -- TODO

baselinesF :: XCommandT
baselinesF = error "SVUser.baselinesF" -- TODO

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
    error "SVUser.nextServer" -- TODO