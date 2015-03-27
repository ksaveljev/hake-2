{-# LANGUAGE TemplateHaskell #-}
module QCommon.LumpT where

import Control.Lens (makeLenses)

data LumpT =
  LumpT { _lFileOfs :: Int
        , _lFileLen :: Int
        }

makeLenses ''LumpT
