module Game.GameCombat
    ( radiusDamage
    ) where

import           Types

radiusDamage :: Ref' EdictT -> Ref' EdictT -> Float -> Maybe (Ref' EdictT) -> Float -> Int -> Quake ()
radiusDamage = error "GameCombat.radiusDamage" -- TODO