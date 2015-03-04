module Game.GameLocalsT where

import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as UV

import Game.GClientT

data GameLocalsT =
  GameLocalsT { glHelpMessage1 :: B.ByteString
              , glHelpMessage2 :: B.ByteString
              , glHelpChanged  :: Int
              , glClients      :: UV.Vector GClientT
              , glSpawnPoint   :: B.ByteString
              , glMaxClients   :: Int
              , glMaxEntities  :: Int
              , glServerFlags  :: Int
              , glNumItems     :: Int
              , glAutosaved    :: Bool
              }
