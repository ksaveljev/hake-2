module Game.GameUtil where

import qualified Data.ByteString as B

import Types
import QuakeState
import Game.Adapters

spawn :: Quake EdictReference
initEdict :: EdictReference -> Quake ()
clearEdict :: EdictReference -> Quake ()
freeEdict :: EdictReference -> Quake ()
range :: EdictT -> EdictT -> Int
useTargets :: EdictReference -> Maybe EdictReference -> Quake ()
freeEdictA :: EntThink
monsterUse :: EntUse
mCheckAttack :: EntThink
killBox :: EdictReference -> Quake Bool
visible :: EdictReference -> EdictReference -> Quake Bool
findTarget :: EdictReference -> Quake Bool
inFront :: EdictT -> EdictT -> Bool
foundTarget :: EdictReference -> Quake ()
attackFinished :: EdictReference -> Float -> Quake ()
onSameTeam :: EdictReference -> EdictReference -> Quake Bool
megaHealthThink :: EntThink
validateSelectedItem :: EdictReference -> Quake ()