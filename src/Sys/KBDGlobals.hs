{-# LANGUAGE TemplateHaskell #-}
module Sys.KBDGlobals
  ( module Sys.KBDGlobals
  ) where

import Types

import Control.Lens (makeLenses)

makeLenses ''KBDGlobals

initialKBDGlobals :: KBDGlobals
initialKBDGlobals =
  KBDGlobals { _kbdMx    = 0
             , _kbdMy    = 0
             , _kbdWinx  = 0
             , _kbdWiny  = 0
             , _kbdWinW2 = 0
             , _kbdWinH2 = 0
             , _kbdX     = 0
             , _kbdY     = 0
             }