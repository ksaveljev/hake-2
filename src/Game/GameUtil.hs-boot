module Game.GameUtil
    ( clearEdict
    , findTarget
    , freeEdict
    , freeEdictA
    , inFront
    , initEdict
    , killBox
    , mCheckAttack
    , megaHealthThink
    , monsterUse
    , range
    , spawn
    , useTargets
    , validateSelectedItem
    , visible
    ) where

import           Types

clearEdict :: Ref EdictT -> Quake ()
findTarget :: Ref EdictT -> Quake Bool
freeEdict :: Ref EdictT -> Quake ()
freeEdictA :: EntThink
inFront :: EdictT -> EdictT -> Bool
initEdict :: Ref EdictT -> Quake ()
killBox :: Ref EdictT -> Quake Bool
mCheckAttack :: EntThink
megaHealthThink :: EntThink
monsterUse :: EntUse
range :: EdictT -> EdictT -> Int
spawn :: Quake (Ref EdictT)
useTargets :: Ref EdictT -> Maybe (Ref EdictT) -> Quake ()
validateSelectedItem :: Ref EdictT -> Quake ()
visible :: Ref EdictT -> Ref EdictT -> Quake Bool
