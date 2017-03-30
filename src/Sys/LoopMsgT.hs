{-# LANGUAGE TemplateHaskell #-}
module Sys.LoopMsgT where

import           Control.Lens    (makeLenses)
import qualified Data.ByteString as B

import           Types

makeLenses ''LoopMsgT

newLoopMsgT :: LoopMsgT
newLoopMsgT = LoopMsgT
    { _lmData    = ""
    , _lmDataLen = 0
    }