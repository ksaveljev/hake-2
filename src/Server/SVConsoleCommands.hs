{-# LANGUAGE OverloadedStrings #-}
module Server.SVConsoleCommands where

import Control.Lens (use)
import Control.Monad (when)
import qualified Data.ByteString as B

import Quake
import QuakeState
import QCommon.XCommandT
import qualified Game.Cmd as Cmd

{-
===============================================================================

OPERATOR CONSOLE ONLY COMMANDS

These commands can only be entered from stdin or by a remote operator datagram
===============================================================================
-}

{-
====================
SV_SetMaster_f

Specify a list of master servers
====================
-}
setMasterF :: XCommandT
setMasterF = undefined -- TODO

{-
==================
SV_SetPlayer

Sets sv_client and sv_player to the player with idnum Cmd.Argv(1)
==================
-}
setPlayer :: Quake ()
setPlayer = undefined -- TODO

{-
===============================================================================

SAVEGAME FILES

===============================================================================
-}

remove :: B.ByteString -> Quake ()
remove = undefined -- TODO

-- Delete save files save/(number)/.
wipeSaveGame :: B.ByteString -> Quake ()
wipeSaveGame = undefined -- TODO

{-
================
CopyFile
================
-}
copyFile :: B.ByteString -> B.ByteString -> Quake ()
copyFile = undefined -- TODO

{-
================
SV_CopySaveGame
================
-}
copySaveGame :: B.ByteString -> B.ByteString -> Quake ()
copySaveGame = undefined -- TODO

{-
==============
SV_WriteLevelFile

==============
-}
writeLevelFile :: Quake ()
writeLevelFile = undefined -- TODO

{-
==============
SV_ReadLevelFile

==============
-}
readLevelFile :: Quake ()
readLevelFile = undefined -- TODO

{-
==============
SV_WriteServerFile

==============
-}
writeServerFile :: Bool -> Quake ()
writeServerFile = undefined -- TODO

{-
==============
SV_ReadServerFile

==============
-}
readServerFile :: Quake ()
readServerFile = undefined -- TODO

{-
==================
SV_DemoMap_f

Puts the server in demo mode on a specific map/cinematic
==================
-}
demoMapF :: XCommandT
demoMapF = undefined -- TODO

{-
==================
SV_GameMap_f

Saves the state of the map just being exited and goes to a new map.

If the initial character of the map string is '*', the next map is
in a new unit, so the current savegame directory is cleared of
map files.

Example:

*inter.cin+jail

Clears the archived maps, plays the inter.cin cinematic, then
goes to map jail.bsp.
==================
-}
gameMapF :: XCommandT
gameMapF = undefined -- TODO

{-
==================
SV_Map_f

Goes directly to a given map without any savegame archiving.
For development work
==================
-}
mapF :: XCommandT
mapF = undefined -- TODO

{-
=====================================================================

  SAVEGAMES

=====================================================================
-}

{-
==============
SV_Loadgame_f

==============
-}
loadGameF :: XCommandT
loadGameF = undefined -- TODO

{-
==============
SV_Savegame_f

==============
-}
saveGameF :: XCommandT
saveGameF = undefined -- TODO

-- ===============================================================
{-
==================
SV_Kick_f

Kick a user off of the server
==================
-}
kickF :: XCommandT
kickF = undefined -- TODO

{-
================
SV_Status_f
================
-}
statusF :: XCommandT
statusF = undefined -- TODO

{-
==================
SV_ConSay_f
==================
-}
conSayF :: XCommandT
conSayF = undefined -- TODO

{-
==================
SV_Heartbeat_f
==================
-}
heartbeatF :: XCommandT
heartbeatF = undefined -- TODO

{-
===========
SV_Serverinfo_f

  Examine or change the serverinfo string
===========
-}
serverInfoF :: XCommandT
serverInfoF = undefined -- TODO

{-
===========
SV_DumpUser_f

Examine all a users info strings
===========
-}
dumpUserF :: XCommandT
dumpUserF = undefined -- TODO

{-
==============
SV_ServerRecord_f

Begins server demo recording.  Every entity and every message will be
recorded, but no playerinfo will be stored.  Primarily for demo merging.
==============
-}
serverRecordF :: XCommandT
serverRecordF = undefined -- TODO

{-
==============
SV_ServerStop_f

Ends server demo recording
==============
-}
serverStopF :: XCommandT
serverStopF = undefined -- TODO

{-
===============
SV_KillServer_f

Kick everyone off, possibly in preparation for a new game

===============
-}
killServerF :: XCommandT
killServerF = undefined -- TODO

{-
===============
SV_ServerCommand_f

Let the game dll handle a command
===============
-}
serverCommandF :: XCommandT
serverCommandF = undefined -- TODO
-- ===========================================================

{-
==================
SV_InitOperatorCommands
==================
-}
initOperatorCommands :: Quake ()
initOperatorCommands = do
    Cmd.addCommand "heartbeat" (Just heartbeatF)
    Cmd.addCommand "kick" (Just kickF)
    Cmd.addCommand "status" (Just statusF)
    Cmd.addCommand "serverinfo" (Just serverInfoF)
    Cmd.addCommand "dumpuser" (Just dumpUserF)
    Cmd.addCommand "map" (Just mapF)
    Cmd.addCommand "demomap" (Just demoMapF)
    Cmd.addCommand "gamemap" (Just gameMapF)
    Cmd.addCommand "setmaster" (Just setMasterF)

    dedicatedValue <- use $ cvarGlobals.dedicated.cvValue

    when (dedicatedValue /= 0) $
      Cmd.addCommand "say" (Just conSayF)

    Cmd.addCommand "serverrecord" (Just serverRecordF)
    Cmd.addCommand "serverstop" (Just serverStopF)
    Cmd.addCommand "save" (Just saveGameF)
    Cmd.addCommand "load" (Just loadGameF)
    Cmd.addCommand "killserver" (Just killServerF)
    Cmd.addCommand "sv" (Just serverCommandF)
