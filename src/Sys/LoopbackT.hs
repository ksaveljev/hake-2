{-# LANGUAGE TemplateHaskell #-}
module Sys.LoopbackT ( module Sys.LoopbackT
                     , module Sys.LoopMsgT
                     ) where

import Control.Lens (makeLenses)
import qualified Data.Vector as V

import Types
import Sys.LoopMsgT

makeLenses ''LoopbackT

-- MAX_LOOPBACK = 4

newLoopbackT :: LoopbackT
newLoopbackT =
  LoopbackT { _lMsgs = V.replicate 4 newLoopMsgT
            , _lGet  = 0
            , _lSend = 0
            }
