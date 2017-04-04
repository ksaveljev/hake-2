{-# LANGUAGE TemplateHaskell #-}
module Globals where

import           Control.Lens         (makeLenses)
import qualified Data.HashMap.Lazy    as HM
import           Data.IORef           (newIORef)
import qualified Data.Sequence        as Seq
import qualified Data.Vector          as V
import qualified Data.Vector.Unboxed  as UV
import           Linear.V3            (V3(..))
import           System.Random        (mkStdGen)
import           System.IO.Unsafe     (unsafePerformIO)

import           Client.CEntityT
import           Client.ClientStateT
import           Client.ClientStaticT
import           Client.ConsoleT
import           Client.VidDefT
import           Client.VRectT
import qualified Constants
import           Game.EntityStateT
import           QCommon.NetAdrT
import           QCommon.SizeBufT
import           Render.DummyRenderer
import           Types

makeLenses ''Globals

initialGlobals :: Globals
initialGlobals = Globals
    { _gCurTime            = unsafePerformIO (newIORef 0)
    , _gCmdWait            = False
    , _gAliasCount         = 0
    , _gCTraces            = 0
    , _gCBrushTraces       = 0
    , _gCPointContents     = 0
    , _gServerState        = 0
    , _gNetMessage         = newSizeBufT
    , _gNetMessageBuffer   = ""
    , _gCmdText            = newSizeBufT
    , _gDeferTextBuf       = ""
    , _gCmdTextBuf         = ""
    , _gCmdAlias           = Seq.empty
    , _gTimeBeforeGame     = 0
    , _gTimeAfterGame      = 0
    , _gTimeBeforeRef      = 0
    , _gTimeAfterRef       = 0
    , _gLogStatsFile       = Nothing
    , _gCls                = newClientStaticT
    , _gCl                 = newClientStateT
    , _gClEntities         = V.replicate Constants.maxEdicts newCEntityT
    , _gClParseEntities    = V.replicate Constants.maxParseEntities (newEntityStateT Nothing)
    , _gUserInfoModified   = False
    , _gCVars              = HM.empty
    , _gCon                = newConsoleT
    , _gVidDef             = newVidDefT
    , _gRenderer           = Just dummyRenderer
    , _gKeyBindings        = V.replicate 256 Nothing
    , _gKeyDown            = UV.replicate 256 False
    , _gChatTeam           = False
    , _gChatBuffer         = ""
    , _gKeyLines           = V.replicate 32 ""
    , _gKeyLinePos         = 0
    , _gEditLine           = 0
    , _gScrVRect           = newVRectT
    , _gSysFrameTime       = 0
    , _gChatBufferLen      = 0
    , _gGunFrame           = 0
    , _gGunModel           = Nothing
    , _gNetFrom            = newNetAdrT
    , _gLogFile            = Nothing
    , _gVec3Origin         = V3 0 0 0
    , _gRnd               = mkStdGen 0 -- must be changed in initialization code
    }