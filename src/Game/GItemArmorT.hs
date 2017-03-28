{-# LANGUAGE TemplateHaskell #-}
module Game.GItemArmorT where

import Control.Lens (makeLenses)

data GItemArmorT =
  GItemArmorT { _giaBaseCount        :: Int
              , _giaMaxCount         :: Int
              , _giaNormalProtection :: Float
              , _giaEnergyProtection :: Float
              , _giaArmor            :: Int
              }

makeLenses ''GItemArmorT
