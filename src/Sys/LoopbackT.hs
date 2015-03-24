{-# LANGUAGE TemplateHaskell #-}
module Sys.LoopbackT where

import Control.Lens (makeLenses)
import qualified Data.Vector as V

import Sys.LoopMsgT

data LoopbackT =
  LoopbackT { _lMsgs :: V.Vector LoopMsgT
            , _lGet  :: Int
            , _lSend :: Int
            }

makeLenses ''LoopbackT

-- MAX_LOOPBACK = 4

newLoopbackT :: LoopbackT
newLoopbackT =
  LoopbackT { _lMsgs = V.replicate 4 newLoopMsgT
            , _lGet  = 0
            , _lSend = 0
            }
