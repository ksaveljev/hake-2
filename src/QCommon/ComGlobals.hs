{-# LANGUAGE TemplateHaskell #-}
module QCommon.ComGlobals
    ( module QCommon.ComGlobals
    ) where

import           Control.Lens    (makeLenses)
import qualified Data.ByteString as B
import qualified Data.Vector     as V

import           Types

makeLenses ''ComGlobals

initialComGlobals :: ComGlobals
initialComGlobals = ComGlobals
    { _cgComArgc       = 0
    , _cgComArgv       = V.empty
    , _cgRecursive     = False
    , _cgMsg           = B.empty
    , _cgDebugContext  = B.empty
    , _cgDebugContext2 = B.empty
    , _cgRdTarget      = 0
    }