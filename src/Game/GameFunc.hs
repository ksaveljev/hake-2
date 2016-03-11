module Game.GameFunc
  ( spFuncButton
  , spFuncConveyor
  , spFuncDoor
  , spFuncDoorRotating
  , spFuncDoorSecret
  , spFuncKillBox
  , spFuncPlat
  , spFuncRotating
  , spFuncTimer
  , spFuncTrain
  , spFuncWater
  , spTriggerElevator
  ) where

import Types

spFuncButton :: EntThink
spFuncButton = error "GameFunc.spFuncButton" -- TODO

spFuncConveyor :: EntThink
spFuncConveyor = error "GameFunc.spFuncConveyor" -- TODO

spFuncDoor :: EntThink
spFuncDoor = error "GameFunc.spFuncDoor" -- TODO

spFuncDoorSecret :: EntThink
spFuncDoorSecret = error "GameFunc.spFuncDoorSecret" -- TODO

spFuncDoorRotating :: EntThink
spFuncDoorRotating = error "GameFunc.spFuncDoorRotating" -- TODO

spFuncKillBox :: EntThink
spFuncKillBox = error "GameFunc.spFuncKillBox" -- TODO

spFuncPlat :: Ref EdictT -> Quake ()
spFuncPlat = error "GameFunc.spFuncPlat" -- TODO

spFuncRotating :: EntThink
spFuncRotating = error "GameFunc.spFuncRotating" -- TODO

spFuncTimer :: Ref EdictT -> Quake ()
spFuncTimer = error "GameFunc.spFuncTimer" -- TODO

spFuncTrain :: Ref EdictT -> Quake ()
spFuncTrain = error "GameFunc.spFuncTrain" -- TODO

spFuncWater :: Ref EdictT -> Quake ()
spFuncWater = error "GameFunc.spFuncWater" -- TODO

spTriggerElevator :: EntThink
spTriggerElevator = error "GameFunc.spTriggerElevator" -- TODO