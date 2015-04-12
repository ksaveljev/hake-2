{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Globals ( module Globals
               , module Client.ClientStaticT
               , module Client.ClientStateT
               , module Client.ConsoleT
               , module Client.RefExportT
               , module Game.CmdAliasT
               , module Game.CVarT
               , module QCommon.SizeBufT
               ) where

import Control.Lens (makeLenses)
import Linear.V3 (V3(..))
import System.Random (mkStdGen)
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import Client.ClientStaticT
import Client.ClientStateT
import Client.ConsoleT
import Client.RefExportT
import Game.CmdAliasT
import Game.CVarT
import QCommon.NetAdrT
import QCommon.SizeBufT

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

          , _userInfoModified   = False

          , _cvarVars           = M.empty
          , _con                = newConsoleT
          , _re                 = dummyRenderer

          , _keyBindings        = V.replicate 256 Nothing
          , _keyDown            = UV.empty
          , _chatTeam           = False
          , _chatBuffer         = ""
          , _keyLines           = V.empty
          , _keyLinePos         = 0
          , _editLine           = 0

          , _netFrom            = newNetAdrT

          , _vec3Origin         = V3 0 0 0

          , _rnd                = mkStdGen 0 -- must be changed in initialization code
          }
