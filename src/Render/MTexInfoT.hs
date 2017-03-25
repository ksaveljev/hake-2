{-# LANGUAGE TemplateHaskell #-}
module Render.MTexInfoT
    ( module Render.MTexInfoT
    ) where

import           Control.Lens (makeLenses)
import           Linear       (V4(..))

import           Types

makeLenses ''MTexInfoT

newMTexInfoT :: MTexInfoT
newMTexInfoT = MTexInfoT
    { _mtiVecs      = (V4 0 0 0 0, V4 0 0 0 0)
    , _mtiFlags     = 0
    , _mtiNumFrames = 0
    , _mtiNext      = Nothing
    , _mtiImage     = Nothing
    }