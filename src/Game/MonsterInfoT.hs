{-# LANGUAGE TemplateHaskell #-}
module Game.MonsterInfoT ( MonsterInfoT(..)
                         , module Game.MonsterInfoT
                         ) where

import Linear.V3 (V3)
import Control.Lens (makeLenses)

import Internal

makeLenses ''MonsterInfoT
