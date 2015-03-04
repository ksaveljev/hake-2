module Game.Spawn where

import qualified Data.ByteString as B

data Spawn =
  Spawn { spawnName  :: B.ByteString
        , spawnSpawn :: IO () -- TODO: ???
        }
