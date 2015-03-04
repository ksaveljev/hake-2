module Game.ClientPersistant where

import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as UV

import Game.GItem

data ClientPersistant = 
  ClientPersistant { clientPersistantUserInfo        :: B.ByteString
                   , clientPersistantNetName         :: B.ByteString
                   , clientPersistantHand            :: Int
                   , clientPersistantConnected       :: Bool
                   , clientPersistantHealth          :: Int
                   , clientPersistantMaxHealth       :: Int
                   , clientPersistantSavedFlags      :: Int
                   , clientPersistantSelectedItem    :: Int
                   , clientPersistantInventory       :: UV.Vector Int
                   , clientPersistantMaxBullets      :: Int
                   , clientPersistantMaxShells       :: Int
                   , clientPersistantMaxRockets      :: Int
                   , clientPersistantMaxGrenades     :: Int
                   , clientPersistantMaxCells        :: Int
                   , clientPersistantMaxSlugs        :: Int
                   , clientPersistantWeapon          :: GItem
                   , clientPersistantLastWeapon      :: GItem
                   , clientPersistantPowerCubes      :: Int
                   , clientPersistantScore           :: Int
                   , clientPersistantGameHelpChanged :: Int
                   , clientPersistantHelpChanged     :: Int
                   , clientPersistantSpectator       :: Bool
                   }
