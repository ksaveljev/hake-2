{-# LANGUAGE TemplateHaskell #-}
module QCommon.PackT where

import           Control.Lens    (makeLenses)
import           System.IO       (Handle)
import qualified Data.ByteString as B
import qualified Data.Map        as M

import           Types
makeLenses ''PackT

newPackT :: PackT
newPackT = PackT "" Nothing "" 0 M.empty
