module Game.GameWeapon where

import Linear (V3)

import Quake
import QuakeState

fireHit :: EdictReference -> V3 Float -> Int -> Int -> Quake Bool
fireHit _ _ _ _ = do
    io (putStrLn "GameWeapon.fireHit") >> undefined -- TODO

fireBlaster :: EdictReference -> V3 Float -> V3 Float -> Int -> Int -> Int -> Bool -> Quake ()
fireBlaster selfRef@(EdictReference selfIdx) start dir damage speed effect hyper = do
    io (putStrLn "GameWeapon.fireBlaster") >> undefined -- TODO
