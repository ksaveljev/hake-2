{-# LANGUAGE TemplateHaskell #-}
module Game.EdictT ( EdictT(..)
                   , module Game.EdictT
                   , module Game.EdictActionT
                   , module Game.EdictOtherT
                   , module Game.EdictTimingT
                   , module Game.EdictMinMaxT
                   , module Game.EdictInfoT
                   , module Game.EdictPhysicsT
                   , module Game.EdictStatusT
                   , module Game.GClientT
                   ) where

import Control.Lens (makeLenses)

import Internal
import Game.EdictActionT
import Game.EdictOtherT
import Game.EdictTimingT
import Game.EdictMinMaxT
import Game.EdictInfoT
import Game.EdictPhysicsT
import Game.EdictStatusT
import Game.GClientT

makeLenses ''EdictT

newEdictT :: EdictT
newEdictT = undefined -- TODO
