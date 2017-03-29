{-# LANGUAGE TemplateHaskell #-}
module QCommon.FileLinkT where

import           Control.Lens    (makeLenses)
import qualified Data.ByteString as B

import           Types

makeLenses ''FileLinkT