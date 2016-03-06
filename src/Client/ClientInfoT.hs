{-# LANGUAGE TemplateHaskell #-}
module Client.ClientInfoT
  ( module Client.ClientInfoT
  ) where

import qualified Constants
import           Types

import           Control.Lens (makeLenses)
import qualified Data.Vector as V

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
