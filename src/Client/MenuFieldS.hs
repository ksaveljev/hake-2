{-# LANGUAGE TemplateHaskell #-}
module Client.MenuFieldS
    ( module Client.MenuFieldS
    ) where

import           Control.Lens       (makeLenses)
import qualified Data.ByteString    as B

import           Client.MenuCommonS
import           Types

makeLenses ''MenuFieldS

newMenuFieldS :: MenuFieldS
newMenuFieldS = MenuFieldS
    { _mflGeneric       = newMenuCommonS
    , _mflBuffer        = B.empty
    , _mflCursor        = 0
    , _mflLength        = 0
    , _mflVisibleLength = 0
    , _mflVisibleOffset = 0
    }