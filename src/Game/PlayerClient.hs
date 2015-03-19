{-# LANGUAGE Rank2Types #-}
module Game.PlayerClient where

import Quake
import QuakeState

-- Called when a player drops from the server. Will not be called between levels. 
clientDisconnect :: EdictTLens -> Quake ()
clientDisconnect = undefined -- TODO
