{-# LANGUAGE TemplateHaskell #-}
module QuakeState
  ( QuakeState
  , module QuakeState
  , module X )
  where

import Client.KeyGlobals as X
import Game.CmdGlobals as X
import Game.CVarT as X
import Globals as X
import QCommon.CmdAliasT as X
import QCommon.CmdFunctionT as X
import QCommon.ComGlobals as X
import QCommon.FileLinkT as X
import QCommon.FSGlobals as X
import QCommon.PackFileT as X
import QCommon.PackT as X
import QCommon.SearchPathT as X
import QCommon.SizeBufT as X
import QCommon.XCommandT as X
import Types

import Control.Lens (makeLenses)

makeLenses ''QuakeState

initialQuakeState :: QuakeState
initialQuakeState =
  QuakeState { _globals    = initialGlobals
             , _comGlobals = initialComGlobals
             , _cmdGlobals = initialCmdGlobals
             , _keyGlobals = initialKeyGlobals
             , _fsGlobals  = initialFSGlobals
             }
