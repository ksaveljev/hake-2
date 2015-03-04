module Game.ClientPersistantT where

import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as UV

import Game.GItemT

data ClientPersistantT = 
  ClientPersistantT { cpUserInfo        :: B.ByteString
                    , cpNetName         :: B.ByteString
                    , cpHand            :: Int
                    , cpConnected       :: Bool
                    , cpHealth          :: Int
                    , cpMaxHealth       :: Int
                    , cpSavedFlags      :: Int
                    , cpSelectedItem    :: Int
                    , cpInventory       :: UV.Vector Int
                    , cpMaxBullets      :: Int
                    , cpMaxShells       :: Int
                    , cpMaxRockets      :: Int
                    , cpMaxGrenades     :: Int
                    , cpMaxCells        :: Int
                    , cpMaxSlugs        :: Int
                    , cpWeapon          :: GItemT
                    , cpLastWeapon      :: GItemT
                    , cpPowerCubes      :: Int
                    , cpScore           :: Int
                    , cpGameHelpChanged :: Int
                    , cpHelpChanged     :: Int
                    , cpSpectator       :: Bool
                    }
