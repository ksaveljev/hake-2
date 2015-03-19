{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Server.ServerT ( ServerT(..)
                      , module Server.ServerT
                      ) where

import Control.Lens (makeLenses)
import qualified Data.Vector as V

import Internal
import Game.CModelT
import Game.EntityStateT
import QCommon.SizeBufT
import qualified Constants

makeLenses ''ServerT

-- configstrings must be initialized to full vector of values
newServerT :: ServerT
newServerT =
  ServerT { _sState         = 0
          , _sAttractLoop   = False
          , _sLoadGame      = False
          , _sTime          = 0
          , _sFrameNum      = 0
          , _sName          = ""
          , _sModels        = V.replicate Constants.maxModels newCModelT
          , _sConfigStrings = V.replicate Constants.maxConfigStrings ""
          , _sBaselines     = V.replicate Constants.maxEdicts newEntityStateT
          , _sMulticast     = newSizeBufT
          , _sMulticastBuf  = ""
          , _sDemoFile      = Nothing
          , _sTimeDemo      = 0
          }
