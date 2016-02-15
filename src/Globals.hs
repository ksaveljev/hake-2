{-# LANGUAGE TemplateHaskell #-}
module Globals
  (module Globals)
  where

import           Types

import           Control.Lens (makeLenses)
import qualified Data.HashMap.Lazy as HM
import           System.Random (mkStdGen)

makeLenses ''Globals

initialGlobals :: Globals
initialGlobals =
  Globals { _gCurTime = 0
          , _gCmdText = SizeBufT False False "" 0 0 0
          , _gCVars   = HM.empty
          , _gRnd     = mkStdGen 0
          }