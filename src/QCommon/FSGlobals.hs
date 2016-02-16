{-# LANGUAGE TemplateHaskell #-}
module QCommon.FSGlobals
  (module QCommon.FSGlobals)
  where

import           Types

import           Control.Lens (makeLenses)
import qualified Data.Sequence as Seq

makeLenses ''FSGlobals

initialFSGlobals :: FSGlobals
initialFSGlobals =
  FSGlobals { _fsGameDir         = ""
            , _fsUserDir         = ""
            , _fsLinks           = Seq.empty
            , _fsSearchPaths     = []
            , _fsBaseSearchPaths = []
            , _fsFileFromPak     = 0
            }