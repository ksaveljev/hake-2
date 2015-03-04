module Game.GItemArmorT where

data GItemArmorT =
  GItemArmorT { giaBaseCount        :: Int
              , giaMaxCount         :: Int
              , giaNormalProtection :: Float
              , giaEnergyProteciton :: Float
              , giaArmor            :: Int
              }
