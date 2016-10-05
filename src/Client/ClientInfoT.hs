{-# LANGUAGE TemplateHaskell #-}
module Client.ClientInfoT
    ( module Client.ClientInfoT
    ) where

import           Control.Lens    (makeLenses)
import qualified Data.ByteString as B
import qualified Data.Vector     as V

import qualified Constants
import           Types

makeLenses ''ClientInfoT

newClientInfoT :: ClientInfoT
newClientInfoT = ClientInfoT
    { _ciName        = B.empty
    , _ciCInfo       = B.empty
    , _ciSkin        = Nothing
    , _ciIcon        = Nothing
    , _ciIconName    = B.empty
    , _ciModel       = Nothing
    , _ciWeaponModel = V.replicate Constants.maxClientWeaponModels Nothing
    }
