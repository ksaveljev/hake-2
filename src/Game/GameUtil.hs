module Game.GameUtil
  ( freeEdict
  , freeEdictA
  , megaHealthThink
  , spawn
  , validateSelectedItem
  ) where

import Types

freeEdict :: Ref EdictT -> Quake ()
freeEdict = error "GameUtil.freeEdict" -- TODO

freeEdictA :: EntThink
freeEdictA = error "GameUtil.freeEdictA" -- TODO

validateSelectedItem :: Ref EdictT -> Quake ()
validateSelectedItem = error "GameUtil.validateSelectedItem" -- TODO

megaHealthThink :: EntThink
megaHealthThink = error "GameUtil.megaHealthThink" -- TODO

spawn :: Quake (Ref EdictT)
spawn = error "GameUtil.spawn" -- TODO