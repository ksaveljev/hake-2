{-# LANGUAGE TemplateHaskell #-}
module QCommon.FSGlobals
    ( module QCommon.FSGlobals
    ) where

import           Control.Lens    (makeLenses)
import qualified Data.ByteString as B
import qualified Data.Sequence   as Seq

import           Types

makeLenses ''FSGlobals

initialFSGlobals :: FSGlobals
initialFSGlobals = FSGlobals
    { _fsGameDir         = B.empty
    , _fsUserDir         = B.empty
    , _fsLinks           = Seq.empty
    , _fsSearchPaths     = []
    , _fsBaseSearchPaths = []
    , _fsFileFromPak     = 0
    }