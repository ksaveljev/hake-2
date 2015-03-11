{-# LANGUAGE TemplateHaskell #-}
module Game.EdictPhysicsT ( EdictPhysicsT(..)
                          , module Game.EdictPhysicsT
                          ) where

import Control.Lens (makeLenses)

import Internal

makeLenses ''EdictPhysicsT
