module Game.GameSpawn
    ( callSpawn
    , spawnEntities
    ) where

import qualified Data.ByteString as B

import           Types

callSpawn :: Ref' EdictT -> Quake ()
spawnEntities :: B.ByteString -> B.ByteString -> B.ByteString -> Quake ()