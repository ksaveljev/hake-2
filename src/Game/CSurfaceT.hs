{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Game.CSurfaceT where

import Control.Lens (makeLenses)
import qualified Data.ByteString as B

data CSurfaceT =
  CSurfaceT { _csName  :: B.ByteString
            , _csFlags :: Int
            , _csValue :: Int
            }

makeLenses ''CSurfaceT

newCSurfaceT :: CSurfaceT
newCSurfaceT =
  CSurfaceT { _csName  = ""
            , _csFlags = 0
            , _csValue = 0
            }
