module Game.PlayerClient
  ( saveClientData
  , spInfoPlayerCoop
  , spInfoPlayerDeathmatch
  , spInfoPlayerIntermission
  , spInfoPlayerStart
  ) where

import Types

saveClientData :: Quake ()
saveClientData = error "PlayerClient.saveClientData" -- TODO

spInfoPlayerCoop :: Ref EdictT -> Quake ()
spInfoPlayerCoop = error "PlayerClient.spInfoPlayerCoop" -- TODO

spInfoPlayerDeathmatch :: Ref EdictT -> Quake ()
spInfoPlayerDeathmatch = error "PlayerClient.spInfoPlayerDeathmatch" -- TODO

spInfoPlayerIntermission :: Quake ()
spInfoPlayerIntermission = error "PlayerClient.spInfoPlayerIntermission" -- TODO

spInfoPlayerStart :: Ref EdictT -> Quake ()
spInfoPlayerStart = error "PlayerClient.spInfoPlayerStart" -- TODO