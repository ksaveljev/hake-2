module Game.GameSpawn
  ( spawnEntities
  ) where

import Types

import qualified Data.ByteString as B

spawnEntities :: B.ByteString -> B.ByteString -> B.ByteString -> Quake ()
spawnEntities = error "GameSpawn.spawnEntities" -- TODO