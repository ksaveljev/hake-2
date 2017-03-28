{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Globals ( module Globals
               , module X
               ) where

import Control.Lens (makeLenses)
import Linear.V3 (V3(..))
import System.Random (mkStdGen)
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import Client.CEntityT as X
import Client.ClientStaticT as X
import Client.ClientStateT as X
import Client.ConsoleT as X
import Client.RefExportT as X
import Client.VidDefT as X
import Client.VRectT as X
import Game.CmdAliasT as X
import Game.CVarT as X
import Game.EntityStateT as X
import QCommon.NetAdrT
import QCommon.SizeBufT as X
import Render.DummyRenderer
import Render.Renderer as X
import qualified Constants

import Internal

makeLenses ''Globals

initialGlobals :: Globals
initialGlobals =
  Globals { _curtime            = 0
          , _cmdWait            = False

          , _aliasCount         = 0
          , _cTraces            = 0
          , _cBrushTraces       = 0
          , _cPointContents     = 0
          , _serverState        = 0
          
          , _netMessage         = newSizeBufT
          , _netMessageBuffer   = ""
          , _cmdText            = newSizeBufT
          , _deferTextBuf       = ""
          , _cmdTextBuf         = ""
          , _cmdAlias           = Seq.empty

          , _timeBeforeGame     = 0
          , _timeAfterGame      = 0
          , _timeBeforeRef      = 0
          , _timeAfterRef       = 0

          , _logStatsFile       = Nothing

          , _cls                = newClientStaticT
          , _cl                 = newClientStateT
          , _clEntities         = V.replicate Constants.maxEdicts newCEntityT
          , _clParseEntities    = V.replicate Constants.maxParseEntities (newEntityStateT Nothing)

          , _userInfoModified   = False

          , _cvarVars           = M.empty
          , _con                = newConsoleT
          , _vidDef             = newVidDefT
          , _re                 = Just dummyRenderer

          , _keyBindings        = V.replicate 256 Nothing
          , _keyDown            = UV.replicate 256 False
          , _chatTeam           = False
          , _chatBuffer         = ""
          , _keyLines           = V.replicate 32 ""
          , _keyLinePos         = 0
          , _editLine           = 0

          , _scrVRect           = newVRectT
          , _sysFrameTime       = 0
          , _chatBufferLen      = 0
          , _gunFrame           = 0
          , _gunModel           = Nothing
          , _netFrom            = newNetAdrT

          , _logFile            = Nothing

          , _vec3Origin         = V3 0 0 0

          , _rnd                = mkStdGen 0 -- must be changed in initialization code
          }
