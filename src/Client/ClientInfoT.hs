{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Client.ClientInfoT ( ClientInfoT(..)
                          , module Client.ClientInfoT
                          ) where

import Control.Lens (makeLenses)
import qualified Data.Vector.Unboxed as UV

import Internal
import qualified Constants

makeLenses ''ClientInfoT

newClientInfoT :: ClientInfoT
newClientInfoT =
  ClientInfoT { _ciName        = ""
              , _ciCInfo       = ""
              , _ciSkin        = Nothing
              , _ciIcon        = Nothing
              , _ciIconName    = ""
              , _ciModel       = Nothing
              , _ciWeaponModel = UV.replicate Constants.maxClientWeaponModels 0
              }
