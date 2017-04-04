{-# LANGUAGE TemplateHaskell #-}
module QCommon.ComGlobals where

import           Control.Lens    (makeLenses)
import qualified Data.ByteString as B
import qualified Data.Vector     as V

import qualified Constants
import           Types

makeLenses ''ComGlobals

initialComGlobals :: ComGlobals
initialComGlobals = ComGlobals
    { _cgComArgc       = 0
    , _cgComArgv       = V.replicate Constants.maxNumArgvs ""
    , _cgRecursive     = False
    , _cgMsg           = B.empty
    , _cgDebugContext  = ""
    , _cgDebugContext2 = ""
    , _cgRdTarget      = 0
    }