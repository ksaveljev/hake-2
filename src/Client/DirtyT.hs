{-# LANGUAGE TemplateHaskell #-}
module Client.DirtyT
    ( module Client.DirtyT
    ) where

import           Control.Lens (makeLenses)

import           Internal

makeLenses ''DirtyT

newDirtyT :: DirtyT
newDirtyT = DirtyT
    { _x1 = 0
    , _x2 = 0
    , _y1 = 0
    , _y2 = 0
    }
