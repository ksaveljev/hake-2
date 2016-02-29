{-# LANGUAGE TemplateHaskell #-}
module QCommon.ComGlobals
  ( module QCommon.ComGlobals
  ) where

import Types

import           Control.Lens (makeLenses)
import qualified Data.Vector as V

makeLenses ''ComGlobals

initialComGlobals :: ComGlobals
initialComGlobals =
  ComGlobals { _cgComArgc       = 0
             , _cgComArgv       = V.empty
             , _cgRecursive     = False
             , _cgMsg           = ""
             , _cgDebugContext  = ""
             , _cgDebugContext2 = ""
             , _cgRdTarget      = 0
             }