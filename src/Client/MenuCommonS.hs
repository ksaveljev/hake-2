{-# LANGUAGE TemplateHaskell #-}
module Client.MenuCommonS
  ( module Client.MenuCommonS
  ) where

import Types

import Control.Lens (makeLenses)
import Linear (V4(..))

makeLenses ''MenuCommonS

newMenuCommonS :: MenuCommonS
newMenuCommonS =
  MenuCommonS { _mcType          = 0
              , _mcName          = Just ""
              , _mcX             = 0
              , _mcY             = 0
              , _mcParent        = Nothing
              , _mcCursorOffset  = 0
              , _mcLocalData     = V4 0 0 0 0
              , _mcFlags         = 0
              , _mcN             = 0
              , _mcStatusBar     = Nothing
              , _mcCallback      = Nothing
              , _mcStatusBarFunc = Nothing
              , _mcOwnerDraw     = Nothing
              , _mcCursorDraw    = Nothing
              }