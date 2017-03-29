{-# LANGUAGE TemplateHaskell #-}
module QCommon.SearchPathT where

import           Control.Lens    (makeLenses)
import qualified Data.ByteString as B

import           Types

makeLenses ''SearchPathT

newSearchPathT :: SearchPathT
newSearchPathT = SearchPathT "" Nothing