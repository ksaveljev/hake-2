{-# LANGUAGE TemplateHaskell #-}
module Game.SpawnT where

import Control.Lens (makeLenses)
import qualified Data.ByteString as B

data SpawnT =
  SpawnT { _sName  :: B.ByteString
         , _sSpawn :: IO () -- TODO: ???
         }

makeLenses ''SpawnT
