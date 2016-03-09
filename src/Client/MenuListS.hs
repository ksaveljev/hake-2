{-# LANGUAGE TemplateHaskell #-}
module Client.MenuListS
  ( module Client.MenuListS
  ) where

import           Client.MenuCommonS
import           Types

import           Control.Lens (makeLenses)
import qualified Data.Vector as V

makeLenses ''MenuListS

newMenuListS :: MenuListS
newMenuListS =
  MenuListS { _mlGeneric   = newMenuCommonS
            , _mlCurValue  = 0
            , _mlItemNames = V.empty
            }