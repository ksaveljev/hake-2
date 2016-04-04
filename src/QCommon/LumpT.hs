{-# LANGUAGE TemplateHaskell #-}
module QCommon.LumpT
  ( module QCommon.LumpT
  ) where

import Types
import Util.Binary (getInt)

import Control.Lens (makeLenses)
import Data.Binary.Get (Get)

makeLenses ''LumpT

getLumpT :: Get LumpT
getLumpT = LumpT <$> getInt <*> getInt