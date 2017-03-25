{-# LANGUAGE TemplateHaskell #-}
module Client.KButtonT
    ( module Client.KButtonT
    ) where

import           Control.Lens (makeLenses)

import           Types

makeLenses ''KButtonT

newKButtonT :: KButtonT
newKButtonT = KButtonT
    { _kbDown     = (0, 0)
    , _kbDownTime = 0
    , _kbMsec     = 0
    , _kbState    = 0
    }