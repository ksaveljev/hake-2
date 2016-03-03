module Game.GameUtil
  ( freeEdict
  , freeEdictA
  , megaHealthThink
  , spawn
  , validateSelectedItem
  ) where

import Types

freeEdict :: EdictRef -> Quake ()
freeEdict = error "GameUtil.freeEdict" -- TODO

freeEdictA :: EntThink
freeEdictA = error "GameUtil.freeEdictA" -- TODO

validateSelectedItem :: EdictRef -> Quake ()
validateSelectedItem = error "GameUtil.validateSelectedItem" -- TODO

megaHealthThink :: EntThink
megaHealthThink = error "GameUtil.megaHealthThink" -- TODO

spawn :: Quake EdictRef
spawn = error "GameUtil.spawn" -- TODO