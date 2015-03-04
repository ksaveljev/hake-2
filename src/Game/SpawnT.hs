module Game.SpawnT where

import qualified Data.ByteString as B

data SpawnT =
  SpawnT { sName  :: B.ByteString
         , sSpawn :: IO () -- TODO: ???
         }
