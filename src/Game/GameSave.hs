{-# LANGUAGE FlexibleContexts #-}
module Game.GameSave
    ( initGame
    , writeGame
    , writeLevel
    ) where

import           Control.Lens          (use, (.=), (^.), (&), (.~))
import           Control.Monad         (unless)
import           Data.Bits             ((.|.))
import qualified Data.ByteString       as B
import           Data.Foldable         (traverse_)
import qualified Data.Vector           as V

import qualified Constants
import           Game.CVarT
import           Game.EdictT
import qualified Game.GameItems        as GameItems
import           Game.GameLocalsT
import           Game.GClientT
import qualified Game.PlayerClient     as PlayerClient
import qualified QCommon.Com           as Com
import           QCommon.CVarVariables
import           QuakeState
import           Types
import qualified Util.QuakeFile        as QuakeFile

initialCVars :: [(B.ByteString, B.ByteString, Int)]
initialCVars =
    [ ("gun_x", "0", 0), ("gun_y", "0", 0), ("gun_z", "0", 0)
    , ("sv_rollspeed", "200", 0), ("sv_rollangle", "2", 0)
    , ("sv_maxvelocity", "2000", 0), ("sv_gravity", "800", 0)
    , ("dedicated", "0", Constants.cvarNoSet)
    , ("cheats", "0", Constants.cvarServerInfo .|. Constants.cvarLatch)
    , ("gamename", Constants.gameVersion, Constants.cvarServerInfo .|. Constants.cvarLatch)
    , ("gamedate", Constants.__date__, Constants.cvarServerInfo .|. Constants.cvarLatch)
    , ("maxclients", "4", Constants.cvarServerInfo .|. Constants.cvarLatch)
    , ("maxspectators", "4", Constants.cvarServerInfo)
    , ("deathmatch", "0", Constants.cvarLatch)
    , ("coop", "0", Constants.cvarLatch)
    , ("skill", "0", Constants.cvarLatch)
    , ("maxentities", "1024", Constants.cvarLatch)
    , ("dmflags", "0", Constants.cvarServerInfo)
    , ("fraglimit", "0", Constants.cvarServerInfo)
    , ("timelimit", "0", Constants.cvarServerInfo)
    , ("password", B.empty, Constants.cvarUserInfo)
    , ("spectator_password", B.empty, Constants.cvarUserInfo)
    , ("needpass", "0", Constants.cvarServerInfo)
    , ("filterban", "1", 0), ("g_select_empty", "0", Constants.cvarArchive)
    , ("run_pitch", "0.002", 0), ("run_roll", "0.005", 0)
    , ("bob_up", "0.005", 0), ("bob_pitch", "0.002", 0)
    , ("bob_roll", "0.002", 0), ("flood_msgs", "4", 0)
    , ("flood_persecond", "4", 0), ("flood_waitdelay", "10", 0)
    , ("sv_maplist", B.empty, 0)
    ]

initGame :: Quake ()
initGame = do
    initializeCVars
    GameItems.initItems
    setHelpMessages
    setMaxEntities =<< maxEntitiesCVar
    createEdicts =<< use (gameBaseGlobals.gbGame.glMaxEntities)
    setMaxClients =<< maxClientsCVar
    createClients =<< use (gameBaseGlobals.gbGame.glMaxClients)
  where
    setHelpMessages = do
        gameBaseGlobals.gbGame.glHelpMessage1 .= B.empty
        gameBaseGlobals.gbGame.glHelpMessage2 .= B.empty
    setMaxEntities maxEntities =
        gameBaseGlobals.gbGame.glMaxEntities .= truncate (maxEntities^.cvValue)
    createEdicts maxEntities = do
        edicts <- io (V.thaw (V.generate (maxEntities + 1) newEdictT)) -- one extra for "dummy edict"
        gameBaseGlobals.gbGEdicts .= edicts -- TODO: this is not correct
    setMaxClients maxClients =
        gameBaseGlobals.gbGame.glMaxClients .= truncate (maxClients^.cvValue)
    createClients maxClients = do
        gameBaseGlobals.gbGame.glClients .= V.generate maxClients newGClientT
        gameBaseGlobals.gbNumEdicts .= maxClients + 1

initializeCVars :: Quake ()
initializeCVars = do
    cvar <- use (gameBaseGlobals.gbGameImport.giCVar)
    mapM_ (\(name, val, flags) -> cvar name val flags) initialCVars

writeLevel :: B.ByteString -> Quake ()
writeLevel = error "GameSave.writeLevel" -- TODO

writeGame :: B.ByteString -> Bool -> Quake ()
writeGame fileName autoSave = do
    unless autoSave $
        PlayerClient.saveClientData
    qf <- QuakeFile.open fileName
    maybe writeGameError doWriteGame qf
  where
    writeGameError = Com.fatalError "GameSave.writeGame QuakeFile is Nothing"
    doWriteGame qf = do
        gameLocals <- use (gameBaseGlobals.gbGame)
        io (QuakeFile.writeGameLocals qf (gameLocals & glAutosaved .~ autoSave))
        gameBaseGlobals.gbGame.glAutosaved .= False
        clients <- use (gameBaseGlobals.gbGame.glClients)
        io (traverse_ (QuakeFile.writeGClient qf) clients)
        QuakeFile.close qf
