{-# LANGUAGE TemplateHaskell #-}
module QCommon.FSGlobals where

import           Control.Lens        (makeLenses)
import qualified Data.Sequence       as Seq

import           QCommon.SearchPathT
import           QCommon.FileLinkT
import           Types

makeLenses ''FSGlobals

initialFSGlobals :: FSGlobals
initialFSGlobals = FSGlobals
    { _fsGameDir         = ""
    , _fsUserDir         = ""
    , _fsLinks           = Seq.empty
    , _fsSearchPaths     = []
    , _fsBaseSearchPaths = []
    , _fsFileFromPak     = 0
    }