{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Client.MenuFieldS where

import Control.Lens (makeLenses)
import qualified Data.ByteString as B

import Client.MenuCommonS

data MenuFieldS =
  MenuFieldS { _mfGeneric       :: MenuCommonS
             , _mfBuffer        :: B.ByteString
             , _mfCursor        :: Int
             , _mfLength        :: Int
             , _mfVisibleLength :: Int
             , _mfVisibleOffset :: Int
             }

makeLenses ''MenuFieldS

newMenuFieldS :: MenuFieldS
newMenuFieldS =
  MenuFieldS { _mfGeneric       = newMenuCommonS
             , _mfBuffer        = ""
             , _mfCursor        = 0
             , _mfLength        = 0
             , _mfVisibleLength = 0
             , _mfVisibleOffset = 0
             }
