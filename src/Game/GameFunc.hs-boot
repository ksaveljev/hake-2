module Game.GameFunc
    ( funcTrainFind
    , spFuncButton
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
    , trainUse
    ) where

import           Types
    
spFuncButton :: EntThink
spFuncConveyor :: EntThink
spFuncDoor :: EntThink
spFuncDoorSecret :: EntThink
spFuncDoorRotating :: EntThink
spFuncKillBox :: EntThink
spFuncPlat :: Ref EdictT -> Quake ()
spFuncRotating :: EntThink
spFuncTimer :: Ref EdictT -> Quake ()
spFuncTrain :: Ref EdictT -> Quake ()
spFuncWater :: Ref EdictT -> Quake ()
spTriggerElevator :: EntThink
trainUse :: EntUse
funcTrainFind :: EntThink