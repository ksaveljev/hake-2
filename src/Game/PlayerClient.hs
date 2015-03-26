{-# LANGUAGE Rank2Types #-}
module Game.PlayerClient where

import Control.Lens (Traversal')

import Quake
import QuakeState

-- Called when a player drops from the server. Will not be called between levels. 
--clientDisconnect :: QuakeLens EdictT -> Quake ()
clientDisconnect :: Traversal' QuakeState EdictT -> Quake ()
clientDisconnect = undefined -- TODO
