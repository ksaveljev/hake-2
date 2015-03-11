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
