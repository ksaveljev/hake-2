module Game.PlayerHud
    ( checkChaseStats
    , deathmatchScoreboardMessage
    , moveClientToIntermission
    , setSpectatorStats
    , setStats
    ) where

import           Types

checkChaseStats :: Ref EdictT -> Quake ()
checkChaseStats = error "PlayerHud.checkChaseStats" -- TODO

deathmatchScoreboardMessage :: Ref EdictT -> Maybe (Ref EdictT) -> Quake ()
deathmatchScoreboardMessage = error "PlayerHud.deathmatchScoreboardMessage" -- TODO

moveClientToIntermission :: Ref EdictT -> Quake ()
moveClientToIntermission = error "PlayerHud.moveClientToIntermission" -- TODO

setSpectatorStats :: Ref EdictT -> Quake ()
setSpectatorStats = error "PlayerHud.setSpectatorStats" -- TODO

setStats :: Ref EdictT -> Quake ()
setStats = error "PlayerHud.setStats" -- TODO
