{-# LANGUAGE TemplateHaskell #-}
module QCommon.PackT where

import           Control.Lens      (makeLenses)
import qualified Data.ByteString   as B
import qualified Data.HashMap.Lazy as HM

import           Types

makeLenses ''PackT

newPackT :: PackT
newPackT = PackT B.empty Nothing B.empty 0 HM.empty