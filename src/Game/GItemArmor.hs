module Game.GItemArmor where

data GItemArmor =
  GItemArmor { gItemArmorBaseCount        :: Int
             , gItemArmorMaxCount         :: Int
             , gItemArmorNormalProtection :: Float
             , gItemArmorEnergyProteciton :: Float
             , gItemArmorArmor            :: Int
             }
