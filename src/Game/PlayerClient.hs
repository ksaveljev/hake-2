{-# LANGUAGE Rank2Types #-}
module Game.PlayerClient where

import Control.Lens (Traversal')

import Quake
import QuakeState

-- Called when a player drops from the server. Will not be called between levels. 
clientDisconnect :: Traversal' QuakeState (Maybe Int) -> Quake ()
clientDisconnect = undefined -- TODO
