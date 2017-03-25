{-# LANGUAGE TemplateHaskell #-}
module Sys.LoopMsgT
    ( module Sys.LoopMsgT
    ) where

import           Control.Lens    (makeLenses)
import qualified Data.ByteString as B

import           Types

makeLenses ''LoopMsgT

newLoopMsgT :: LoopMsgT
newLoopMsgT = LoopMsgT
    { _lmData    = B.empty
    , _lmDataLen = 0
    }