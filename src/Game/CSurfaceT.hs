module Game.CSurfaceT where

import qualified Data.ByteString as B

data CSurfaceT =
  CSurfaceT { csName  :: B.ByteString
            , csFlags :: Int
            , csValue :: Int
            }
