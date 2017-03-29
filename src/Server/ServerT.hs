{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Server.ServerT ( ServerT(..)
                      , module Server.ServerT
                      , module Game.CModelT
                      , module Game.EntityStateT
                      , module QCommon.SizeBufT
                      ) where

import Control.Lens (makeLenses)
import qualified Data.Vector as V

import Types
import Game.CModelT
import Game.EntityStateT
import QCommon.SizeBufT
import qualified Constants

makeLenses ''ServerT

newServerT :: ServerT
newServerT =
  ServerT { _sState         = 0
          , _sAttractLoop   = False
          , _sLoadGame      = False
          , _sTime          = 0
          , _sFrameNum      = 0
          , _sName          = ""
          , _sModels        = V.replicate Constants.maxModels (CModelReference (-1))
          , _sConfigStrings = V.replicate Constants.maxConfigStrings ""
          , _sBaselines     = V.replicate Constants.maxEdicts (newEntityStateT Nothing)
          , _sMulticast     = newSizeBufT
          , _sMulticastBuf  = ""
          , _sDemoFile      = Nothing
          , _sTimeDemo      = 0
          }
