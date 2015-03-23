{-# LANGUAGE TemplateHaskell #-}
module Client.DirtyT where

import Control.Lens (makeLenses)

data DirtyT =
  DirtyT { _x1 :: Int
         , _x2 :: Int
         , _y1 :: Int
         , _y2 :: Int
         }

makeLenses ''DirtyT

newDirtyT :: DirtyT
newDirtyT =
  DirtyT { _x1 = 0
         , _x2 = 0
         , _y1 = 0
         , _y2 = 0
         }
