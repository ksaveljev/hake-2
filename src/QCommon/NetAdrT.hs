{-# LANGUAGE TemplateHaskell #-}
module QCommon.NetAdrT where

import Control.Lens (makeLenses)
import Network.Socket (HostAddress)

import qualified Constants

data NetAdrT =
  NetAdrT { _naType :: Int
          , _naPort :: Int
          , _naIP   :: Maybe HostAddress
          }

makeLenses ''NetAdrT

newNetAdrT :: NetAdrT
newNetAdrT = NetAdrT Constants.naLoopback 0 Nothing
