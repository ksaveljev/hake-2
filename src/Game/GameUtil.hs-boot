module Game.GameUtil where

import qualified Data.ByteString as B

import Types
import QuakeState
import Game.Adapters

spawn :: Quake (Ref EdictT)
initEdict :: (Ref EdictT) -> Quake ()
clearEdict :: (Ref EdictT) -> Quake ()
freeEdict :: (Ref EdictT) -> Quake ()
range :: EdictT -> EdictT -> Int
useTargets :: (Ref EdictT) -> Maybe (Ref EdictT) -> Quake ()
freeEdictA :: EntThink
monsterUse :: EntUse
mCheckAttack :: EntThink
killBox :: (Ref EdictT) -> Quake Bool
visible :: (Ref EdictT) -> (Ref EdictT) -> Quake Bool
findTarget :: (Ref EdictT) -> Quake Bool
inFront :: EdictT -> EdictT -> Bool
foundTarget :: (Ref EdictT) -> Quake ()
attackFinished :: (Ref EdictT) -> Float -> Quake ()
onSameTeam :: (Ref EdictT) -> (Ref EdictT) -> Quake Bool
megaHealthThink :: EntThink
validateSelectedItem :: (Ref EdictT) -> Quake ()
