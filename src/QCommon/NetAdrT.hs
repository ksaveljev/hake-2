module QCommon.NetAdrT where

import Data.Word (Word32)

data NetAdrT =
  NetAdrT { naType :: Int
          , naPort :: Int
          , naIP   :: Word32 -- TODO: how to represent?
          }
