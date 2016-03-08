{-# LANGUAGE TemplateHaskell #-}
module QCommon.DPackHeaderT
  ( module QCommon.DPackHeaderT
  ) where

import Types
import Util.Binary (getInt)

import Control.Lens (makeLenses)
import Data.Binary (Get)

makeLenses ''DPackHeaderT

getDPackHeaderT :: Get DPackHeaderT
getDPackHeaderT = DPackHeaderT <$> getInt <*> getInt <*> getInt 