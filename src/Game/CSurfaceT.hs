{-# LANGUAGE TemplateHaskell #-}
module Game.CSurfaceT where

import Control.Lens (makeLenses)
import qualified Data.ByteString as B

data CSurfaceT =
  CSurfaceT { _csName  :: B.ByteString
            , _csFlags :: Int
            , _csValue :: Int
            }

makeLenses ''CSurfaceT
