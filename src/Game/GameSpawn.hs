{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
module Game.GameSpawn where

import Control.Lens (use, (^.), (.=), (%=))
import Control.Monad (liftM, when, void)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V

import Quake
import QuakeState
import CVarVariables
import qualified Game.PlayerClient as PlayerClient
import qualified QCommon.Com as Com

{-
- SpawnEntities
- 
- Creates a server's entity / program execution context by parsing textual
- entity definitions out of an ent file.
-}
spawnEntities :: B.ByteString -> B.ByteString -> B.ByteString -> Quake ()
spawnEntities mapName entities spawnPoint = do
    Com.dprintf $ "SpawnEntities(), mapname=" `B.append` mapName `B.append` "\n"

    -- avoiding the "defaulting the following constraints" warning is fun
    skillValue :: Float <- liftM (fromIntegral . ((floor . (^.cvValue)) :: CVarT -> Int)) skillCVar
    let skillLevel = if | skillValue < 0 -> 0
                        | skillValue > 3 -> 3
                        | otherwise -> skillValue

    when (skillValue /= skillLevel) $ do
      forceSet <- use $ gameBaseGlobals.gbGameImport.giCVarForceSet
      void (forceSet "skill" (BC.pack $ show skillLevel))

    PlayerClient.saveClientData

    gameBaseGlobals.gbLevel .= newLevelLocalsT

    maxEntities <- use $ gameBaseGlobals.gbGame.glMaxEntities
    let newEdicts = map (\i -> (i, newEdictT i)) [0..maxEntities-1]
    gameBaseGlobals.gbGEdicts %= (V.// newEdicts)

    gameBaseGlobals.gbLevel.llMapName .= mapName
    gameBaseGlobals.gbGame.glSpawnPoint .= spawnPoint

    maxClients <- use $ gameBaseGlobals.gbGame.glMaxClients
    edicts <- use $ gameBaseGlobals.gbGEdicts
    let updatedEdicts = V.imap (\idx edict -> if idx >= 1 && idx <= maxClients then edict { _eClient = Just (idx - 1) } else edict) edicts
    gameBaseGlobals.gbGEdicts .= updatedEdicts

    io (putStrLn "GameSpawn.spawnEntities") >> undefined -- TODO
