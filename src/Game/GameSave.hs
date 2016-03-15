{-# LANGUAGE FlexibleContexts #-}
module Game.GameSave
  ( initGame
  ) where

import qualified Constants
import           Game.CVarT
import           Game.EdictT
import qualified Game.GameItems as GameItems
import           Game.GameLocalsT
import           Game.GClientT
import           QCommon.CVarVariables
import           QuakeIOState
import           QuakeState
import           Types

import           Control.Lens (use, (.=), (^.))
import           Data.Bits ((.|.))
import qualified Data.ByteString as B
import qualified Data.Vector as V

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
initGame =
  do initializeCVars
     GameItems.initItems
     setHelpMessages
     setMaxEntities =<< maxEntitiesCVar
     createEdicts =<< use (gameBaseGlobals.gbGame.glMaxEntities)
     setMaxClients =<< maxClientsCVar
     createClients =<< use (gameBaseGlobals.gbGame.glMaxClients)
  where setHelpMessages =
          do gameBaseGlobals.gbGame.glHelpMessage1 .= B.empty
             gameBaseGlobals.gbGame.glHelpMessage2 .= B.empty
        setMaxEntities maxEntities =
          gameBaseGlobals.gbGame.glMaxEntities .= truncate (maxEntities^.cvValue)
        createEdicts maxEntities = request $
          do edicts <- io (V.thaw (V.generate (maxEntities + 1) newEdictT)) -- one extra for "dummy edict"
             gbGEdicts .= edicts
        setMaxClients maxClients =
          gameBaseGlobals.gbGame.glMaxClients .= truncate (maxClients^.cvValue)
        createClients maxClients =
          gameBaseGlobals.gbGame.glClients .= V.generate maxClients newGClientT

initializeCVars :: Quake ()
initializeCVars =
  do cvar <- use (gameBaseGlobals.gbGameImport.giCVar)
     mapM_ (\(name, val, flags) -> cvar name val flags) initialCVars
