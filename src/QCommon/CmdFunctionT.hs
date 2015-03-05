{-# LANGUAGE TemplateHaskell #-}
module QCommon.CmdFunctionT where

import Control.Lens (makeLenses)
import qualified Data.ByteString as B

import QCommon.XCommandT

data CmdFunctionT =
  CmdFunctionT { _cfName     :: B.ByteString
               , _cfFunction :: XCommandT
               }

makeLenses ''CmdFunctionT
