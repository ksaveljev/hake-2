{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE Rank2Types #-}
module Server.SVMain where

import Data.Bits ((.|.), (.&.))
import Data.Maybe (isJust)
import Data.Traversable (traverse)
import Control.Lens (use, (.=), (%=), (^.))
import Control.Monad (void, when, liftM)
import System.IO (hClose)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V

import Quake
import QuakeState
import CVarVariables
import QCommon.NetAdrT
import qualified Constants
import qualified Game.PlayerClient as PlayerClient
import qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import qualified QCommon.MSG as MSG
import qualified QCommon.NetChannel as NetChannel
import qualified QCommon.SZ as SZ
import {-# SOURCE #-} qualified Server.SVConsoleCommands as SVConsoleCommands
import qualified Server.SVEnts as SVEnts
import qualified Server.SVGame as SVGame
import qualified Server.SVSend as SVSend
import qualified Sys.NET as NET
import qualified Util.Lib as Lib

{-
- Send a message to the master every few minutes to let it know we are
- alive, and log information.
-}
heartbeatSeconds :: Int
heartbeatSeconds = 300

-- only called at quake2.exe startup, not for each game
init :: Quake ()
init = do
    SVConsoleCommands.initOperatorCommands

    void $ CVar.get "rcon_password" "" 0

    void $ CVar.get "skill" "1" 0
    void $ CVar.get "deathmatch" "0" Constants.cvarLatch
    void $ CVar.get "coop" "0" Constants.cvarLatch
    void $ CVar.get "dmflags" (BC.pack $ show Constants.dfInstantItems) Constants.cvarServerInfo -- IMPROVE: convert Int to ByteString using binary package?
    void $ CVar.get "fraglimit" "0" Constants.cvarServerInfo
    void $ CVar.get "timelimit" "0" Constants.cvarServerInfo
    void $ CVar.get "cheats" "0" (Constants.cvarServerInfo .|. Constants.cvarLatch)
    void $ CVar.get "protocol" (BC.pack $ show Constants.protocolVersion) (Constants.cvarServerInfo .|. Constants.cvarNoSet) -- IMPROVE: convert Int to ByteString using binary package?

    void $ CVar.get "maxclients" "1" (Constants.cvarServerInfo .|. Constants.cvarLatch)
    void $ CVar.get "hostname" "noname" (Constants.cvarServerInfo .|. Constants.cvarArchive)
    void $ CVar.get "timeout" "125" 0
    void $ CVar.get "zombietime" "2" 0
    void $ CVar.get "showclamp" "0" 0
    void $ CVar.get "paused" "0" 0
    void $ CVar.get "timedemo" "0" 0
    void $ CVar.get "sv_enforcetime" "0" 0
    void $ CVar.get "allow_download" "1" Constants.cvarArchive
    void $ CVar.get "allow_download_players" "0" Constants.cvarArchive
    void $ CVar.get "allow_download_models" "1" Constants.cvarArchive
    void $ CVar.get "allow_download_sounds" "1" Constants.cvarArchive
    void $ CVar.get "allow_download_maps" "1" Constants.cvarArchive
    void $ CVar.get "sv_noreload" "0" 0
    void $ CVar.get "sv_airaccelerate" "0" Constants.cvarLatch
    void $ CVar.get "public" "0" 0
    void $ CVar.get "sv_reconnect_limit" "3" Constants.cvarArchive

    bufData <- use $ globals.netMessageBuffer
    SZ.init (globals.netMessage) bufData Constants.maxMsglen

-- Called when each game quits, before Sys_Quit or Sys_Error.
shutdown :: B.ByteString -> Bool -> Quake ()
shutdown finalmsg reconnect = do
    clients <- use $ svGlobals.svServerStatic.ssClients

    when (V.null clients) $ 
      finalMessage finalmsg reconnect

    masterShutdown

    SVGame.shutdownGameProgs

    -- free current level
    sDemofile <- use $ svGlobals.svServer.sDemoFile
    when (isJust sDemofile) $ do
      let Just h = sDemofile
      io $ hClose h -- IMPROVE: catch exception

    let newServer = newServerT
    svGlobals.svServer .= newServer
    globals.serverState .= (newServer^.sState)

    ssDemofile <- use $ svGlobals.svServerStatic.ssDemoFile
    when (isJust ssDemofile) $ do
      let Just h = ssDemofile
      io $ hClose h -- IMPROVE: catch exception

    svGlobals.svServerStatic .= newServerStaticT

{-
- Used by SV_Shutdown to send a final message to all connected clients
- before the server goes down. The messages are sent immediately, not just
- stuck on the outgoing message list, because the server is going to
- totally exit after returning from this function.
-}
finalMessage :: B.ByteString -> Bool -> Quake ()
finalMessage = undefined -- TODO

-- Master_Shutdown, Informs all masters that this server is going down.
masterShutdown :: Quake ()
masterShutdown = undefined -- TODO

{-
- Called when the player is totally leaving the server, either willingly or
- unwillingly. This is NOT called if the entire server is quiting or
- crashing.
-}
dropClient :: QuakeLens ClientT -> Quake ()
dropClient clientLens = do
    client <- use $ clientLens

    MSG.writeByteI (clientLens.cNetChan.ncMessage) Constants.svcDisconnect

    when ((client^.cState) == Constants.csSpawned) $
      PlayerClient.clientDisconnect (clientLens.cEdict)

    when ((client^.cDownload) /= "") $
      clientLens.cDownload .= ""

    clientLens.cState .= Constants.csZombie
    clientLens.cName .= ""

{- ==============================================================================
- 
- CONNECTIONLESS COMMANDS
- 
- ==============================================================================-}

{-
 - Builds the string that is sent as heartbeats and status replies.
 -}
statusString :: Quake B.ByteString
statusString = undefined -- TODO

frame :: Int -> Quake ()
frame msec = do
    globals.timeBeforeGame .= 0
    globals.timeAfterGame .= 0

    -- if server is not active, do nothing
    initialized <- use $ svGlobals.svServerStatic.ssInitialized

    when initialized $ do
      svGlobals.svServerStatic.ssRealTime %= (+ msec)

      -- keep the random time dependent
      void Lib.rand

      -- check timeouts
      checkTimeouts

      -- get packets from clients
      readPackets

      -- move autonomous things around if enough time has passed
      undefined -- TODO

      -- update ping based on the last known frame from all clients
      calcPings

      -- give the clients some timeslices
      giveMsec

      -- let everything in the world think and move
      runGameFrame

      -- send messages back to the clients that had packets read this frame
      SVSend.sendClientMessages

      -- save the entire world state if recording a serverdemo
      SVEnts.recordDemoMessage

      -- send a heartbeat to the master if needed
      masterHeartbeat

      -- clear teleport flags, etc for next frame
      prepWorldFrame

{-
- If a packet has not been received from a client for timeout.value
- seconds, drop the conneciton. Server frames are used instead of realtime
- to avoid dropping the local client while debugging.
- 
- When a client is normally dropped, the client_t goes into a zombie state
- for a few seconds to make sure any final reliable message gets resent if
- necessary.
-}
checkTimeouts :: Quake ()
checkTimeouts = undefined -- TODO

-- Reads packets from the network or loopback
readPackets :: Quake ()
readPackets = undefined -- TODO

-- Updates the cl.ping variables
calcPings :: Quake ()
calcPings = undefined -- TODO

{-
- Every few frames, gives all clients an allotment of milliseconds for
- their command moves. If they exceed it, assume cheating.
-}
giveMsec :: Quake ()
giveMsec = do
    frameNum <- use $ svGlobals.svServer.sFrameNum

    when (frameNum .&. 15 == 0) $
      svGlobals.svServerStatic.ssClients %=
        fmap (\cl -> if (cl^.cState) == Constants.csFree
                      then cl
                      else cl { _cCommandMsec = 1800 }) -- 1600 + some slop


runGameFrame :: Quake ()
runGameFrame = undefined -- TODO

masterHeartbeat :: Quake ()
masterHeartbeat = do
    dedicatedValue <- liftM (^.cvValue) dedicatedCVar
    publicServerValue <- liftM (^.cvValue) publicServerCVar


    -- only dedicated servers send heartbeats
    -- no need if it is a private dedicated game
    when (dedicatedValue /= 0 || publicServerValue == 0) $ do
      lastHeartbeat <- use $ svGlobals.svServerStatic.ssLastHeartbeat
      realtime <- use $ svGlobals.svServerStatic.ssRealTime

      if | lastHeartbeat > realtime -> svGlobals.svServerStatic.ssLastHeartbeat .= realtime
         | realtime - lastHeartbeat < heartbeatSeconds * 1000 -> return () -- not time to send yet
         | otherwise -> do
             svGlobals.svServerStatic.ssLastHeartbeat .= realtime

             -- send the same string that we would give for a status OOB command
             str <- statusString

             -- send to group master
             masterAdr <- use $ svGlobals.svMasterAdr
             void $ traverse (sendToNetAdr str) masterAdr

  where sendToNetAdr :: B.ByteString -> NetAdrT -> Quake ()
        sendToNetAdr strToSend netAdr = do
          Com.printf $ "Sending heartbeat to " `B.append` NET.adrToString netAdr `B.append` "\n"
          NetChannel.outOfBandPrint Constants.nsServer netAdr ("heartbeat\n" `B.append` strToSend)

{-
- SV_PrepWorldFrame
- 
- This has to be done before the world logic, because player processing
- happens outside RunWorldFrame.
-}
prepWorldFrame :: Quake ()
prepWorldFrame = undefined -- TODO
