{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Game.ClientPersistantT ( ClientPersistantT(..)
                              , module Game.ClientPersistantT
                              ) where

import Control.Lens (makeLenses)
import qualified Data.Vector.Unboxed as UV

import Types
import qualified Constants

makeLenses ''ClientPersistantT

newClientPersistantT :: ClientPersistantT
newClientPersistantT =
  ClientPersistantT { _cpUserInfo        = ""
                    , _cpNetName         = ""
                    , _cpHand            = 0
                    , _cpConnected       = False
                    , _cpHealth          = 0
                    , _cpMaxHealth       = 0
                    , _cpSavedFlags      = 0
                    , _cpSelectedItem    = 0
                    , _cpInventory       = UV.replicate Constants.maxItems 0
                    , _cpMaxBullets      = 0
                    , _cpMaxShells       = 0
                    , _cpMaxRockets      = 0
                    , _cpMaxGrenades     = 0
                    , _cpMaxCells        = 0
                    , _cpMaxSlugs        = 0
                    , _cpWeapon          = Nothing
                    , _cpLastWeapon      = Nothing
                    , _cpPowerCubes      = 0
                    , _cpScore           = 0
                    , _cpGameHelpChanged = 0
                    , _cpHelpChanged     = 0
                    , _cpSpectator       = False
                    }
