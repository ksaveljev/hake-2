{-# LANGUAGE OverloadedStrings #-}
module Server.SVMain where

import Data.Bits ((.|.))
import Control.Lens (use)
import Control.Monad (void)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Quake
import QuakeState
import qualified Constants
import qualified QCommon.CVar as CVar
import qualified QCommon.SZ as SZ
import {-# SOURCE #-} qualified Server.SVConsoleCommands as SVConsoleCommands

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
svFrame msec = return () -- undefined -- TODO
