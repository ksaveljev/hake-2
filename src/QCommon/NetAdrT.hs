{-# LANGUAGE TemplateHaskell #-}
module QCommon.NetAdrT where

import Data.Word (Word32)
import Control.Lens (makeLenses)

data NetAdrT =
  NetAdrT { _naType :: Int
          , _naPort :: Int
          , _naIP   :: Word32 -- TODO: how to represent?
          }

makeLenses ''NetAdrT
