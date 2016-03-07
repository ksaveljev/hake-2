{-# LANGUAGE TemplateHaskell #-}
module Server.ServerT
  ( module Server.ServerT
  ) where

import qualified Constants
import           Game.EntityStateT (newEntityStateT)
import           QCommon.SizeBufT (newSizeBufT)
import           Types

import           Control.Lens (makeLenses)
import qualified Data.Vector as V

makeLenses ''ServerT

newServerT :: ServerT
newServerT =
  ServerT { _sState         = 0
          , _sAttractLoop   = False
          , _sLoadGame      = False
          , _sTime          = 0
          , _sFrameNum      = 0
          , _sName          = ""
          , _sModels        = V.replicate Constants.maxModels (Ref (-1))
          , _sConfigStrings = V.replicate Constants.maxConfigStrings ""
          , _sBaselines     = V.replicate Constants.maxEdicts (newEntityStateT Nothing)
          , _sMulticast     = newSizeBufT
          , _sMulticastBuf  = ""
          , _sDemoFile      = Nothing
          , _sTimeDemo      = 0
          }