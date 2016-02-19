{-# LANGUAGE TemplateHaskell #-}
module QCommon.NetAdrT
  ( module QCommon.NetAdrT
  ) where

import qualified Constants
import           Types

import           Control.Lens (makeLenses)

makeLenses ''NetAdrT

newNetAdrT :: NetAdrT
newNetAdrT = NetAdrT Constants.naLoopback 0 (Just 0) -- TODO: this.ip = InetAddress.getByName(null).getAddress(); // localhost / 127.0.0.1