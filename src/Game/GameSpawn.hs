module Game.GameSpawn where

import qualified Data.ByteString as B

import Quake

{-
- SpawnEntities
- 
- Creates a server's entity / program execution context by parsing textual
- entity definitions out of an ent file.
-}
spawnEntities :: B.ByteString -> B.ByteString -> B.ByteString -> Quake ()
spawnEntities _ _ _ = io (putStrLn "GameSpawn.spawnEntities") >> undefined -- TODO
