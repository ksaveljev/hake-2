{-# LANGUAGE TemplateHaskell #-}
module QCommon.DPackHeaderT
    ( module QCommon.DPackHeaderT
    ) where

import           Control.Lens (makeLenses)
import           Data.Binary  (Get)

import           Types
import           Util.Binary  (getInt)

makeLenses ''DPackHeaderT

getDPackHeaderT :: Get DPackHeaderT
getDPackHeaderT = DPackHeaderT <$> getInt <*> getInt <*> getInt 