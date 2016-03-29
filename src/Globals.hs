{-# LANGUAGE TemplateHaskell #-}
module Globals
  ( module Globals
  ) where

import           Client.ClientStateT (newClientStateT)
import           Client.ClientStaticT (newClientStaticT)
import           Client.ConsoleT (newConsoleT)
import           Client.VidDefT (newVidDefT)
import           Client.VRectT (newVRectT)
import           QCommon.NetAdrT (newNetAdrT)
import           QCommon.SizeBufT (newSizeBufT)
import           Types

import           Control.Lens (makeLenses)
import qualified Data.ByteString as B
import qualified Data.HashMap.Lazy as HM
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import           Linear (V3(..))
import           Render.DummyRenderer (dummyRenderer)
import           System.Random (mkStdGen)

makeLenses ''Globals

initialGlobals :: Globals
initialGlobals =
  Globals { _gCmdWait          = False
          , _gAliasCount       = 0
          , _gCTraces          = 0
          , _gCBrushTraces     = 0
          , _gCPointContents   = 0
          , _gServerState      = 0
          , _gNetMessage       = newSizeBufT
          , _gCmdText          = newSizeBufT
          , _gDeferTextBuf     = B.empty
          , _gCmdAlias         = Seq.empty
          , _gTimeBeforeGame   = 0
          , _gTimeAfterGame    = 0
          , _gLogStatsFile     = Nothing
          , _gCls              = newClientStaticT
          , _gCl               = newClientStateT
          , _gUserInfoModified = False
          , _gCVars            = HM.empty
          , _gCon              = newConsoleT
          , _gVidDef           = newVidDefT
          , _gRenderer         = Just dummyRenderer
          , _gKeyBindings      = V.replicate 256 Nothing
          , _gKeyLines         = V.replicate 32 B.empty
          , _gKeyLinePos       = 0
          , _gEditLine         = 0
          , _gScrVRect         = newVRectT
          , _gSysFrameTime     = 0
          , _gGunFrame         = 0
          , _gGunModel         = Nothing
          , _gNetFrom          = newNetAdrT
          , _gLogFile          = Nothing
          , _gVec3Origin       = V3 0 0 0
          , _gRnd              = mkStdGen 0
          }
