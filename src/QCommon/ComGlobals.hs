{-# LANGUAGE TemplateHaskell #-}
module QCommon.ComGlobals ( ComGlobals
                          , initialComGlobals
                          , cgComArgc
                          , cgComArgv
                          , cgRecursive
                          , cgMsg
                          ) where

import Control.Lens (makeLenses)
import qualified Data.ByteString as B
import qualified Data.Vector as V

import Internal

makeLenses ''ComGlobals

initialComGlobals :: ComGlobals
initialComGlobals =
  ComGlobals { _cgComArgc   = 0
             , _cgComArgv   = V.empty
             , _cgRecursive = False
             , _cgMsg       = B.empty
             }