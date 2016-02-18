{-# LANGUAGE TemplateHaskell #-}
module Globals
  ( module Globals
  ) where

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
          , _gServerState      = 0
          , _gNetMessage       = SizeBufT False False "" 0 0 0
          , _gCmdText          = SizeBufT False False "" 0 0 0
          , _gCmdAlias         = Seq.empty
          , _gUserInfoModified = False
          , _gCVars            = HM.empty
          , _gKeyBindings      = V.replicate 256 Nothing
          , _gKeyLines         = V.replicate 32 ""
          , _gKeyLinePos       = 0
          , _gRnd              = mkStdGen 0
          }