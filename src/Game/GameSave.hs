{-# LANGUAGE OverloadedStrings #-}
module Game.GameSave where

import Data.Bits ((.|.))
import Control.Lens (use, (^.), (.=))
import Control.Monad (void, liftM, when)
import qualified Data.ByteString as B
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import Quake
import QuakeState
import CVarVariables
import Util.QuakeFile (QuakeFile)
import qualified Constants
import qualified Game.GameItems as GameItems
import qualified Util.QuakeFile as QuakeFile

writeLevel :: B.ByteString -> Quake ()
writeLevel fileName = do
    qf <- io $ QuakeFile.open fileName -- IMPROVE: catch exception
    -- if (f == null)
    --     GameBase.gi.error("Couldn't open for writing: " + filename);
    
    level <- use $ gameBaseGlobals.gbLevel
    numEdicts <- use $ gameBaseGlobals.gbNumEdicts
    edicts <- use $ gameBaseGlobals.gbGEdicts
    
    io $ do
        -- write out level_locals_t
        QuakeFile.writeLevelLocals qf level
        -- write out all the entities
        writeEdicts qf edicts 0 numEdicts
        QuakeFile.writeInt qf (-1)
        QuakeFile.close qf
    
  where writeEdicts :: QuakeFile -> MV.IOVector EdictT -> Int -> Int -> IO ()
        writeEdicts qf edicts idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              edict <- MV.read edicts idx
              
              when (edict^.eInUse) $ do
                QuakeFile.writeInt qf idx
                QuakeFile.writeEdict qf edict
            
              writeEdicts qf edicts (idx + 1) maxIdx

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

writeGame :: B.ByteString -> Bool -> Quake ()
writeGame fileName autosave = do
    io (putStrLn "GameSave.writeGame") >> undefined -- TODO
