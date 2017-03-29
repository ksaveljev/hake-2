{-# LANGUAGE TemplateHaskell #-}
module QCommon.LumpT where

import Control.Lens (makeLenses)

import Types

data LumpT =
  LumpT { _lFileOfs :: Int
        , _lFileLen :: Int
        }

makeLenses ''LumpT
