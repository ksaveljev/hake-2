module Game.GameItems
  ( bodyArmorInfo
  , combatArmorInfo
  , dropAmmo
  , dropGeneral
  , dropPowerArmor
  , findItem
  , jacketArmorInfo
  , pickupAdrenaline
  , pickupAmmo
  , pickupAncientHead
  , pickupArmor
  , pickupBandolier
  , pickupHealth
  , pickupKey
  , pickupPack
  , pickupPowerArmor
  , pickupPowerup
  , useBreather
  , useEnviroSuit
  , useInvulnerability
  , usePowerArmor
  , useQuad
  , useSilencer
  ) where

import qualified Constants
import           Types

import qualified Data.ByteString as B

jacketArmorInfo :: GItemArmorT
jacketArmorInfo =
  GItemArmorT { _giaBaseCount        = 25
              , _giaMaxCount         = 50
              , _giaNormalProtection = 0.3
              , _giaEnergyProtection = 0
              , _giaArmor            = Constants.armorJacket
              }

combatArmorInfo :: GItemArmorT
combatArmorInfo =
  GItemArmorT { _giaBaseCount        = 50
              , _giaMaxCount         = 100
              , _giaNormalProtection = 0.6
              , _giaEnergyProtection = 0.3
              , _giaArmor            = Constants.armorCombat
              }

bodyArmorInfo :: GItemArmorT
bodyArmorInfo =
  GItemArmorT { _giaBaseCount        = 100
              , _giaMaxCount         = 200
              , _giaNormalProtection = 0.8
              , _giaEnergyProtection = 0.6
              , _giaArmor            = Constants.armorBody
              }

pickupArmor :: EntInteract
pickupArmor = error "GameItems.pickupArmor" -- TODO

pickupPowerArmor :: EntInteract
pickupPowerArmor = error "GameItems.pickupPowerArmor" -- TODO

usePowerArmor :: ItemUse
usePowerArmor = error "GameItems.usePowerArmor" -- TODO

dropPowerArmor :: ItemDrop
dropPowerArmor = error "GameItems.dropPowerArmor" -- TODO

pickupAmmo :: EntInteract
pickupAmmo = error "GameItems.pickupAmmo" -- TODO

dropAmmo :: ItemDrop
dropAmmo = error "GameItems.dropAmmo" -- TODO

useQuad :: ItemUse
useQuad = error "GameItems.useQuad" -- TODO

pickupPowerup :: EntInteract
pickupPowerup = error "GameItems.pickupPowerup" -- TODO

dropGeneral :: ItemDrop
dropGeneral = error "GameItems.dropGeneral" -- TODO

useInvulnerability :: ItemUse
useInvulnerability = error "GameItems.useInvulnerability" -- TODO

useSilencer :: ItemUse
useSilencer = error "GameItems.useSilencer" -- TODO

useBreather :: ItemUse
useBreather = error "GameItems.useBreather" -- TODO

useEnviroSuit :: ItemUse
useEnviroSuit = error "GameItems.useEnviroSuit" -- TODO

pickupAncientHead :: EntInteract
pickupAncientHead = error "GameItems.pickupAncientHead" -- TODO

pickupAdrenaline :: EntInteract
pickupAdrenaline = error "GameItems.pickupAdrenaline" -- TODO

pickupBandolier :: EntInteract
pickupBandolier = error "GameItems.pickupBandolier" -- TODO

pickupPack :: EntInteract
pickupPack = error "GameItems.pickupPack" -- TODO

pickupKey :: EntInteract
pickupKey = error "GameItems.pickupKey" -- TODO

pickupHealth :: EntInteract
pickupHealth = error "GameItems.pickupHealth" -- TODO

findItem :: B.ByteString -> Quake (Maybe GItemRef)
findItem = error "GameItems.findItem" -- TODO