{-# LANGUAGE TemplateHaskell #-}
module Game.Monsters.MInsaneGlobals where

import           Control.Lens        (makeLenses)
import qualified Data.Vector.Unboxed as UV

import           Types

makeLenses ''MInsaneGlobals

initialMInsaneGlobals :: MInsaneGlobals
initialMInsaneGlobals = MInsaneGlobals
    { _mInsaneSoundFist   = 0
    , _mInsaneSoundShake  = 0
    , _mInsaneSoundMoan   = 0
    , _mInsaneSoundScream = UV.replicate 8 0
    }
