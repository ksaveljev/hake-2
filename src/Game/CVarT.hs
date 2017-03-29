{-# LANGUAGE TemplateHaskell #-}
module Game.CVarT where

import           Control.Lens    (makeLenses)
import qualified Data.ByteString as B

import           Types

makeLenses ''CVarT

newCVarT :: CVarT
newCVarT = CVarT "" "" Nothing 0 False 0.0
