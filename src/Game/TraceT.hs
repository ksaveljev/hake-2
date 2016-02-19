{-# LANGUAGE TemplateHaskell #-}
module Game.TraceT
  ( module Game.TraceT
  ) where

import Types

import Control.Lens (makeLenses)

makeLenses ''TraceT