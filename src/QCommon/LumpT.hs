{-# LANGUAGE TemplateHaskell #-}
module QCommon.LumpT where

import           Control.Lens    (makeLenses)
import           Data.Binary.Get (Get)

import           Types
import           Util.Binary     (getInt)


makeLenses ''LumpT

getLumpT :: Get LumpT
getLumpT = LumpT <$> getInt <*> getInt