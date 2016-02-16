{-# LANGUAGE TemplateHaskell #-}
module Globals
  (module Globals)
  where

import           Types

import           Control.Lens (makeLenses)
import qualified Data.HashMap.Lazy as HM
import qualified Data.Vector as V
import           System.Random (mkStdGen)

makeLenses ''Globals

initialGlobals :: Globals
initialGlobals =
  Globals { _gCurTime    = 0
          , _gCmdWait    = False
          , _gAliasCount = 0
          , _gCmdText    = SizeBufT False False "" 0 0 0
          , _gCVars      = HM.empty
          , _gKeyLines   = V.replicate 32 ""
          , _gKeyLinePos = 0
          , _gRnd        = mkStdGen 0
          }