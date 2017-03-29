{-# LANGUAGE TemplateHaskell #-}
module Client.VidModeT where

import           Control.Lens    (makeLenses)
import qualified Data.ByteString as B

import           Types

makeLenses ''VidModeT