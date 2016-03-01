{-# LANGUAGE TemplateHaskell #-}
module Globals
  ( module Globals
  ) where

import           Client.ClientStaticT (newClientStaticT)
import           Client.ConsoleT (newConsoleT)
import           QCommon.SizeBufT (newSizeBufT)
import           Types

import           Control.Lens (makeLenses)
import qualified Data.HashMap.Lazy as HM
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import           System.Random (mkStdGen)

makeLenses ''Globals

initialGlobals :: Globals
initialGlobals =
  Globals { _gCurTime          = 0
          , _gCmdWait          = False
          , _gAliasCount       = 0
          , _gCTraces          = 0
          , _gCBrushTraces     = 0
          , _gCPointContents   = 0
          , _gServerState      = 0
          , _gNetMessage       = newSizeBufT
          , _gCmdText          = newSizeBufT
          , _gCmdAlias         = Seq.empty
          , _gLogStatsFile     = Nothing
          , _gCls              = newClientStaticT
          , _gUserInfoModified = False
          , _gCVars            = HM.empty
          , _gCon              = newConsoleT
          , _gKeyBindings      = V.replicate 256 Nothing
          , _gKeyLines         = V.replicate 32 ""
          , _gKeyLinePos       = 0
          , _gRnd              = mkStdGen 0
          }