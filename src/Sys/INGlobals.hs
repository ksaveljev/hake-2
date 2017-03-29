{-# LANGUAGE TemplateHaskell #-}
module Sys.INGlobals ( module Sys.INGlobals
                     ) where

import Control.Lens (makeLenses)

import Types

makeLenses ''INGlobals

initialINGlobals :: INGlobals
initialINGlobals =
  INGlobals { _inMouseAvail          = True
            , _inMouseActive         = False
            , _inIgnoreFirst         = False
            , _inMouseButtonState    = 0
            , _inMouseOldButtonState = 0
            , _inOldMouseX           = 0
            , _inOldMouseY           = 0
            , _inMLooking            = False
            }
