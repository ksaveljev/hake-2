{-# LANGUAGE TemplateHaskell #-}
module Game.ClientPersistantT where

import Control.Lens (makeLenses)
import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as UV

import Game.GItemT

data ClientPersistantT = 
  ClientPersistantT { _cpUserInfo        :: B.ByteString
                    , _cpNetName         :: B.ByteString
                    , _cpHand            :: Int
                    , _cpConnected       :: Bool
                    , _cpHealth          :: Int
                    , _cpMaxHealth       :: Int
                    , _cpSavedFlags      :: Int
                    , _cpSelectedItem    :: Int
                    , _cpInventory       :: UV.Vector Int
                    , _cpMaxBullets      :: Int
                    , _cpMaxShells       :: Int
                    , _cpMaxRockets      :: Int
                    , _cpMaxGrenades     :: Int
                    , _cpMaxCells        :: Int
                    , _cpMaxSlugs        :: Int
                    , _cpWeapon          :: GItemT
                    , _cpLastWeapon      :: GItemT
                    , _cpPowerCubes      :: Int
                    , _cpScore           :: Int
                    , _cpGameHelpChanged :: Int
                    , _cpHelpChanged     :: Int
                    , _cpSpectator       :: Bool
                    }

makeLenses ''ClientPersistantT
