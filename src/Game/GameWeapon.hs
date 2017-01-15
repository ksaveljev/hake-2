module Game.GameWeapon
    ( fireHit
    ) where

import           Linear (V3)

import           Types

fireHit :: Ref' EdictT -> V3 Float -> Int -> Int -> Quake Bool
fireHit = error "GameWeapon.fireHit" -- TODO