{-# LANGUAGE TemplateHaskell #-}
module Client.ClientInfoT where

import Control.Lens (makeLenses)
import qualified Data.Vector as V
import qualified Data.ByteString as B

data ClientInfoT =
  ClientInfoT { _ciName        :: B.ByteString
              , _ciCInfo       :: B.ByteString
              , _ciSkin        :: ImageT
              , _ciIcon        :: ImageT
              , _ciIconName    :: B.ByteString
              , _ciModel       :: ModelT
              , _ciWeaponModel :: V.Vector ModelT
              }

makeLenses ''ClientInfoT
