{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Globals ( module Globals
               , module Client.CEntityT
               , module Client.ClientStaticT
               , module Client.ClientStateT
               , module Client.ConsoleT
               , module Client.RefExportT
               , module Client.VidDefT
               , module Client.VRectT
               , module Game.CmdAliasT
               , module Game.CVarT
               , module QCommon.SizeBufT
               , module Render.Renderer
               ) where

import Control.Lens (makeLenses)
import Linear.V3 (V3(..))
import System.Random (mkStdGen)
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import Client.CEntityT
import Client.ClientStaticT
import Client.ClientStateT
import Client.ConsoleT
import Client.RefExportT
import Client.VidDefT
import Client.VRectT
import Game.CmdAliasT
import Game.CVarT
import QCommon.NetAdrT
import QCommon.SizeBufT
import Render.DummyRenderer
import Render.Renderer
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

          , _userInfoModified   = False

          , _cvarVars           = M.empty
          , _con                = newConsoleT
          , _vidDef             = newVidDefT
          , _re                 = Just dummyRenderer

          , _keyBindings        = V.replicate 256 Nothing
          , _keyDown            = UV.empty
          , _chatTeam           = False
          , _chatBuffer         = ""
          , _keyLines           = V.replicate 32 ""
          , _keyLinePos         = 0
          , _editLine           = 0

          , _scrVRect           = newVRectT
          , _sysFrameTime       = 0
          , _netFrom            = newNetAdrT

          , _vec3Origin         = V3 0 0 0

          , _rnd                = mkStdGen 0 -- must be changed in initialization code
          }
