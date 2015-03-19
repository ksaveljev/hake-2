{-# LANGUAGE OverloadedStrings #-}
module Server.SVMain where

import Data.Bits ((.|.))
import Control.Lens (use, (.=), (%=))
import Control.Monad (void, when)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Quake
import QuakeState
import qualified Constants
import qualified QCommon.CVar as CVar
import qualified QCommon.SZ as SZ
import {-# SOURCE #-} qualified Server.SVConsoleCommands as SVConsoleCommands
import qualified Server.SVEnts as SVEnts
import qualified Server.SVSend as SVSend
import qualified Util.Lib as Lib

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
shutdown = undefined -- TODO

{-
- Called when the player is totally leaving the server, either willingly or
- unwillingly. This is NOT called if the entire server is quiting or
- crashing.
-}
dropClient :: ClientT -> Quake ()
dropClient = undefined -- TODO

svFrame :: Int -> Quake ()
svFrame msec = do
    globals.timeBeforeGame .= 0
    globals.timeAfterGame .= 0

    -- if server is not active, do nothing
    initialized <- use $ svGlobals.svServerStatic.ssInitialized

    when initialized $ do
      svGlobals.svServerStatic.ssRealTime %= (+ msec)

      -- keep the random time dependent
      void $ Lib.rand

      -- check timeouts
      svCheckTimeouts

      -- get packets from clients
      svReadPackets

      -- move autonomous things around if enough time has passed
      undefined -- TODO

      -- update ping based on the last known frame from all clients
      svCalcPings

      -- give the clients some timeslices
      svGiveMsec

      -- let everything in the world think and move
      svRunGameFrame

      -- send messages back to the clients that had packets read this frame
      SVSend.svSendClientMessages

      -- save the entire world state if recording a serverdemo
      SVEnts.svRecordDemoMessage

      -- send a heartbeat to the master if needed
      masterHeartbeat

      -- clear teleport flags, etc for next frame
      svPrepWorldFrame

{-
- If a packet has not been received from a client for timeout.value
- seconds, drop the conneciton. Server frames are used instead of realtime
- to avoid dropping the local client while debugging.
- 
- When a client is normally dropped, the client_t goes into a zombie state
- for a few seconds to make sure any final reliable message gets resent if
- necessary.
-}
svCheckTimeouts :: Quake ()
svCheckTimeouts = undefined -- TODO

-- Reads packets from the network or loopback
svReadPackets :: Quake ()
svReadPackets = undefined -- TODO

-- Updates the cl.ping variables
svCalcPings :: Quake ()
svCalcPings = undefined -- TODO

{-
- Every few frames, gives all clients an allotment of milliseconds for
- their command moves. If they exceed it, assume cheating.
-}
svGiveMsec :: Quake ()
svGiveMsec = undefined -- TODO

svRunGameFrame :: Quake ()
svRunGameFrame = undefined -- TODO

masterHeartbeat :: Quake ()
masterHeartbeat = undefined -- TODO

{-
- SV_PrepWorldFrame
- 
- This has to be done before the world logic, because player processing
- happens outside RunWorldFrame.
-}
svPrepWorldFrame :: Quake ()
svPrepWorldFrame = undefined -- TODO
