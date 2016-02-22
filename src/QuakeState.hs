{-# LANGUAGE TemplateHaskell #-}
module QuakeState
  ( QuakeState
  , module QuakeState
  , module X )
  where

import Client.ClientStaticT as X
import Client.KeyGlobals as X
import Game.ClientPersistantT as X
import Game.ClientRespawnT as X
import Game.CmdAliasT as X
import Game.CmdGlobals as X
import Game.CModelT as X
import Game.CPlaneT as X
import Game.CSurfaceT as X
import Game.CVarT as X
import Game.EntityStateT as X
import Game.GameBaseGlobals as X
import {-# SOURCE #-} Game.GameImportT as X
import Game.GameLocalsT as X
import Game.GClientT as X
import Game.GItemArmorT as X
import Game.GItemT as X
import Game.LevelLocalsT as X
import Game.LinkT as X
import Game.MFrameT as X
import Game.MMoveT as X
import Game.MonsterInfoT as X
import Game.MoveInfoT as X
import Game.PlayerStateT as X
import Game.PMoveT as X
import Game.PMoveStateT as X
import Game.PushedT as X
import Game.SpawnTempT as X
import Game.TraceT as X
import Game.UserCmdT as X
import Globals as X
import QCommon.CmdFunctionT as X
import QCommon.ComGlobals as X
import QCommon.FileLinkT as X
import QCommon.FSGlobals as X
import QCommon.NetAdrT as X
import QCommon.NetChanT as X
import QCommon.PackFileT as X
import QCommon.PackT as X
import QCommon.SearchPathT as X
import QCommon.SizeBufT as X
import QCommon.XCommandT as X
import Server.AreaNodeT as X
import Server.ChallengeT as X
import Server.ClientFrameT as X
import Server.ServerStaticT as X
import Server.ServerT as X
import Server.SVGlobals as X
import Types

import Control.Lens (makeLenses)

makeLenses ''QuakeState

initialQuakeState :: QuakeState
initialQuakeState =
  QuakeState { _globals         = initialGlobals
             , _comGlobals      = initialComGlobals
             , _cmdGlobals      = initialCmdGlobals
             , _keyGlobals      = initialKeyGlobals
             , _fsGlobals       = initialFSGlobals
             , _svGlobals       = initialSVGlobals
             , _gameBaseGlobals = initialGameBaseGlobals
             }
