{-# LANGUAGE TemplateHaskell #-}
module Server.ServerT where

import Data.Word (Word8)
import Control.Lens (makeLenses)
import qualified Data.ByteString as B
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import Game.CModelT
import Game.EntityStateT
import QCommon.SizeBufT

data ServerT =
  ServerT { _sState         :: Int
          , _sAttractLoop   :: Bool
          , _sLoadGame      :: Bool
          , _sTime          :: Int
          , _sFrameNum      :: Int
          , _sName          :: B.ByteString
          , _sModels        :: UV.Vector CModelT
          , _sConfigStrings :: V.Vector B.ByteString
          , _sBaselines     :: UV.Vector EntityStateT
          , _sMulticast     :: SizeBufT
          , _sMulticastBuf  :: UV.Vector Word8
          , _sDemoFile      :: B.ByteString
          , _sTimeDemo      :: Int
          }

makeLenses ''ServerT
