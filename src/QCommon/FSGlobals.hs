{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module QCommon.FSGlobals ( module QCommon.FSGlobals
                         , module QCommon.SearchPathT
                         , module QCommon.FileLinkT
                         ) where

import Control.Lens (makeLenses)
import qualified Data.Sequence as Seq

import Internal
import Game.CVarT
import QCommon.SearchPathT
import QCommon.FileLinkT

makeLenses ''FSGlobals

initialFSGlobals :: FSGlobals
initialFSGlobals =
  FSGlobals { _fsGameDir         = ""
            , _fsUserDir         = ""
            , _fsBaseDir         = newCVarT
            , _fsCDDir           = newCVarT
            , _fsGameDirVar      = newCVarT
            , _fsLinks           = Seq.empty
            , _fsSearchPaths     = []
            , _fsBaseSearchPaths = []
            }
