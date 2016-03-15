{-# LANGUAGE TemplateHaskell #-}
module Sys.LoopMsgT
  ( module Sys.LoopMsgT
  ) where

import           Types

import           Control.Lens (makeLenses)
import qualified Data.ByteString as B

makeLenses ''LoopMsgT

newLoopMsgT :: LoopMsgT
newLoopMsgT =
  LoopMsgT { _lmData    = B.empty
           , _lmDataLen = 0
           }
