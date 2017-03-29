{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Client.ClientInfoT ( ClientInfoT(..)
                          , module Client.ClientInfoT
                          ) where

import Control.Lens (makeLenses)
import qualified Data.Vector as V

import Types
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
              , _ciWeaponModel = V.replicate Constants.maxClientWeaponModels Nothing
              }
