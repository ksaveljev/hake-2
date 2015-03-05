{-# LANGUAGE TemplateHaskell #-}
module QCommon.ComGlobals where

import Control.Lens (makeLenses)
import qualified Data.ByteString as B
import qualified Data.Vector as V

data ComGlobals =
  ComGlobals { _cgComArgc   :: Int
             , _cgComArgv   :: V.Vector B.ByteString
             , _cgRecursive :: Bool
             , _cgMsg       :: B.ByteString
             }

makeLenses ''ComGlobals

defaultComGlobals :: ComGlobals
defaultComGlobals =
  ComGlobals { _cgComArgc   = 0
             , _cgComArgv   = V.empty
             , _cgRecursive = False
             , _cgMsg       = B.empty
             }
