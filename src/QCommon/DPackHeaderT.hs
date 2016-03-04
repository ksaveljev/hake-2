{-# LANGUAGE TemplateHaskell #-}
module QCommon.DPackHeaderT
  ( module QCommon.DPackHeaderT
  ) where

import Types
import Util.Binary (getInt)

import Control.Lens (makeLenses)
import Data.Binary (Get)

makeLenses ''DPackHeaderT

getDPackHeader :: Get DPackHeaderT
getDPackHeader = DPackHeaderT <$> getInt <*> getInt <*> getInt 