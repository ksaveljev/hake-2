{-# LANGUAGE TemplateHaskell #-}
module Sys.KBDGlobals ( module Sys.KBDGlobals
                      ) where

import Control.Lens (makeLenses)

import Internal

makeLenses ''KBDGlobals

initialKBDGlobals :: KBDGlobals
initialKBDGlobals =
  KBDGlobals { _kbdMx    = 0
             , _kbdMy    = 0
             , _kbdWinx  = 0
             , _kbdWiny  = 0
             }
