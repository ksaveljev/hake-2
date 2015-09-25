{-# LANGUAGE OverloadedStrings #-}
module Game.GameSave where

import Data.Bits ((.|.))
import Control.Lens (use, (^.), (.=))
import Control.Monad (void, liftM)
import qualified Data.ByteString as B
import qualified Data.Vector as V

import Quake
import QuakeState
import CVarVariables
import qualified Constants
import qualified Game.GameItems as GameItems

writeLevel :: B.ByteString -> Quake ()
writeLevel _ = io (putStrLn "GameSave.writeLevel") >> undefined -- TODO

readLevel :: B.ByteString -> Quake ()
readLevel _ = io (putStrLn "GameSave.readLevel") >> undefined -- TODO

{-
- InitGame
- 
- This will be called when the dll is first loaded, which only happens when
- a new game is started or a save game is loaded. 
-}
initGame :: Quake ()
initGame = do
    gi <- use $ gameBaseGlobals.gbGameImport

    let cvar = gi^.giCVar -- CVar.get

    void $ cvar "gun_x" "0" 0
    void $ cvar "gun_y" "0" 0
    void $ cvar "gun_z" "0" 0

    void $ cvar "sv_rollspeed" "200" 0
    void $ cvar "sv_rollangle" "2" 0
    void $ cvar "sv_maxvelocity" "2000" 0
    void $ cvar "sv_gravity" "800" 0

    -- noset vars
    void $ cvar "dedicated" "0" Constants.cvarNoSet

    -- latched vars
    void $ cvar "cheats" "0" (Constants.cvarServerInfo .|. Constants.cvarLatch)
    void $ cvar "gamename" Constants.gameVersion (Constants.cvarServerInfo .|. Constants.cvarLatch)
    void $ cvar "gamedate" Constants.__date__ (Constants.cvarServerInfo .|. Constants.cvarLatch)

    void $ cvar "maxclients" "4" (Constants.cvarServerInfo .|. Constants.cvarLatch)
    void $ cvar "maxspectators" "4" Constants.cvarServerInfo
    void $ cvar "deathmatch" "0" Constants.cvarLatch
    void $ cvar "coop" "0" Constants.cvarLatch
    void $ cvar "skill" "0" Constants.cvarLatch
    void $ cvar "maxentities" "1024" Constants.cvarLatch

    -- change anytime vars
    void $ cvar "dmflags" "0" Constants.cvarServerInfo
    void $ cvar "fraglimit" "0" Constants.cvarServerInfo
    void $ cvar "timelimit" "0" Constants.cvarServerInfo
    void $ cvar "password" "" Constants.cvarUserInfo
    void $ cvar "spectator_password" "" Constants.cvarUserInfo
    void $ cvar "needpass" "0" Constants.cvarServerInfo
    void $ cvar "filterban" "1" 0

    void $ cvar "g_select_empty" "0" Constants.cvarArchive

    void $ cvar "run_pitch" "0.002" 0
    void $ cvar "run_roll" "0.005" 0
    void $ cvar "bob_up" "0.005" 0
    void $ cvar "bob_pitch" "0.002" 0
    void $ cvar "bob_roll" "0.002" 0
    
    -- flood control
    void $ cvar "flood_msgs" "4" 0
    void $ cvar "flood_persecond" "4" 0
    void $ cvar "flood_waitdelay" "10" 0

    -- dm map list
    void $ cvar "sv_maplist" "" 0
    
    -- items
    GameItems.initItems

    gameBaseGlobals.gbGame.glHelpMessage1 .= ""
    gameBaseGlobals.gbGame.glHelpMessage2 .= ""

    -- initialize all entities for this game
    maxEntitiesValue <- liftM (truncate . (^.cvValue)) maxEntitiesCVar
    gameBaseGlobals.gbGame.glMaxEntities .= maxEntitiesValue

    createEdicts

    -- initialize all clients for this game
    maxClientsValue <- liftM (truncate . (^.cvValue)) maxClientsCVar
    gameBaseGlobals.gbGame.glMaxClients .= maxClientsValue

    createClients

    gameBaseGlobals.gbNumEdicts .= maxClientsValue + 1

createClients :: Quake ()
createClients = do
    maxClients <- use $ gameBaseGlobals.gbGame.glMaxClients
    gameBaseGlobals.gbGame.glClients .= V.generate maxClients newGClientT

createEdicts :: Quake ()
createEdicts = do
    maxEntities <- use $ gameBaseGlobals.gbGame.glMaxEntities
    edicts <- io $ V.thaw $ V.generate (maxEntities + 1) newEdictT -- one extra for "dummy edict"
    gameBaseGlobals.gbGEdicts .= edicts
