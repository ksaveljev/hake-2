{-# LANGUAGE TemplateHaskell #-}
module Client.KButtonT where

import Data.Int (Int64)
import Control.Lens (makeLenses)

data KButtonT =
  KButtonT { _kbDown     :: (Int, Int)
           , _kbDownTime :: Int64
           , _kbMsec     :: Int64
           , _kbState    :: Int
           }

makeLenses ''KButtonT

newKButtonT :: KButtonT
newKButtonT =
  KButtonT { _kbDown     = (0, 0)
           , _kbDownTime = 0
           , _kbMsec     = 0
           , _kbState    = 0
           }
