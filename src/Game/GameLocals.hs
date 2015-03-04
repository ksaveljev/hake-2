module Game.GameLocals where

import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as UV

import Game.GClient

data GameLocals =
  GameLocals { gameLocalsHelpMessage1 :: B.ByteString
             , gameLocalsHelpMessage2 :: B.ByteString
             , gameLocalsHelpChanged  :: Int
             , gameLocalsClients      :: UV.Vector GClient
             , gameLocalsSpawnPoint   :: B.ByteString
             , gameLocalsMaxClients   :: Int
             , gameLocalsMaxEntities  :: Int
             , gameLocalsServerFlags  :: Int
             , gameLocalsNumItems     :: Int
             , gameLocalsAutosaved    :: Bool
             }
