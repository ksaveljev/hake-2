module Sys.IN
  ( initialize
  ) where

import qualified Constants
import qualified QCommon.CVar as CVar
import           Types

import qualified Data.ByteString as B

initialCVars :: [(B.ByteString, B.ByteString, Int)]
initialCVars = [ ("in_mouse", "1", Constants.cvarArchive)
               , ("in_joystick", "0", Constants.cvarArchive)
               ]

initialize :: Quake ()
initialize = CVar.initializeCVars initialCVars
