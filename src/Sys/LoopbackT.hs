{-# LANGUAGE TemplateHaskell #-}
module Sys.LoopbackT
  ( module Sys.LoopbackT
  ) where

import           Sys.LoopMsgT (newLoopMsgT)
import           Types

import           Control.Lens (makeLenses)
import qualified Data.Vector as V

makeLenses ''LoopbackT

newLoopbackT :: LoopbackT
newLoopbackT =
  LoopbackT { _lMsgs = V.replicate 4 newLoopMsgT
            , _lGet  = 0
            , _lSend = 0
            }
