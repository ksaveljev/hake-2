{-# LANGUAGE TemplateHaskell #-}
module Game.GameLocalsT where

import Control.Lens (makeLenses)
import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as UV

import Game.GClientT

data GameLocalsT =
  GameLocalsT { _glHelpMessage1 :: B.ByteString
              , _glHelpMessage2 :: B.ByteString
              , _glHelpChanged  :: Int
              , _glClients      :: UV.Vector GClientT
              , _glSpawnPoint   :: B.ByteString
              , _glMaxClients   :: Int
              , _glMaxEntities  :: Int
              , _glServerFlags  :: Int
              , _glNumItems     :: Int
              , _glAutosaved    :: Bool
              }

makeLenses ''GameLocalsT
