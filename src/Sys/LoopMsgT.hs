{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Sys.LoopMsgT where

import Control.Lens (makeLenses)
import qualified Data.ByteString as B

data LoopMsgT =
  LoopMsgT { _lmData    :: B.ByteString -- max len is Constants.maxMsgLen
           , _lmDataLen :: Int
           }

makeLenses ''LoopMsgT

newLoopMsgT :: LoopMsgT
newLoopMsgT =
  LoopMsgT { _lmData    = ""
           , _lmDataLen = 0
           }
