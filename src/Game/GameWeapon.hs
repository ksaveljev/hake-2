module Game.GameWeapon
    ( fireBlaster
    , fireHit
    ) where

import           Linear (V3)

import           Types

fireBlaster :: Ref EdictT -> V3 Float -> V3 Float -> Int -> Int -> Int -> Bool -> Quake ()
fireBlaster = error "GameWeapon.fireBlaster" -- TODO

fireHit :: Ref EdictT -> V3 Float -> Int -> Int -> Quake Bool
fireHit = error "GameWeapon.fireHit" -- TODO