{-# LANGUAGE TemplateHaskell #-}
module QCommon.LumpT where

import Control.Lens (makeLenses)

data LumpT =
  LumpT { _lFileofs :: Int
        , _lFilelen :: Int
        }

makeLenses ''LumpT
