{-# LANGUAGE OverloadedStrings #-}
module Game.GameFunc where

import Quake
import QuakeState
import Game.EntThink

spFuncButton :: EntThink
spFuncButton =
  GenericEntThink "sp_func_button" $ \_ -> do
    io (putStrLn "GameFunc.spFuncButton") >> undefined -- TODO

spFuncDoor :: EntThink
spFuncDoor =
  GenericEntThink "sp_func_door" $ \_ -> do
    io (putStrLn "GameFunc.spFuncDoor") >> undefined -- TODO

spFuncDoorSecret :: EntThink
spFuncDoorSecret =
  GenericEntThink "sp_func_door_secret" $ \_ -> do
    io (putStrLn "GameFunc.spFuncDoorSecret") >> undefined -- TODO

spFuncDoorRotating :: EntThink
spFuncDoorRotating =
  GenericEntThink "sp_func_door_rotating" $ \_ -> do
    io (putStrLn "GameFunc.spFuncDoorRotating") >> undefined -- TODO

spFuncConveyor :: EntThink
spFuncConveyor =
  GenericEntThink "sp_func_conveyor" $ \_ -> do
    io (putStrLn "GameFunc.spFuncConveyor") >> undefined -- TODO

spFuncKillBox :: EntThink
spFuncKillBox =
  GenericEntThink "sp_func_killbox" $ \_ -> do
    io (putStrLn "GameFunc.spFuncKillBox") >> undefined -- TODO

spFuncRotating :: EntThink
spFuncRotating =
  GenericEntThink "sp_func_rotating" $ \_ -> do
    io (putStrLn "GameFunc.spFuncRotating") >> undefined -- TODO

spTriggerElevator :: EntThink
spTriggerElevator =
  GenericEntThink "sp_trigger_elevator" $ \_ -> do
    io (putStrLn "GameFunc.spTriggerElevator") >> undefined -- TODO

spFuncPlat :: EdictReference -> Quake ()
spFuncPlat _ = io (putStrLn "GameFunc.spFuncPlat") >> undefined -- TODO

spFuncWater :: EdictReference -> Quake ()
spFuncWater _ = io (putStrLn "GameFunc.spFuncWater") >> undefined -- TODO

spFuncTrain :: EdictReference -> Quake ()
spFuncTrain _ = io (putStrLn "GameFunc.spFuncTrain") >> undefined -- TODO

spFuncTimer :: EdictReference -> Quake ()
spFuncTimer _ = io (putStrLn "GameFunc.spFuncTimer") >> undefined -- TODO
