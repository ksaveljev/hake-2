module QCommon.CVarShared where

import           QuakeState
import           Types

import           Control.Lens (use, (^.))
import qualified Data.ByteString as B
import qualified Data.HashMap.Lazy as HM

findVar :: B.ByteString -> Quake (Maybe CVarT)
findVar name =
  do vars <- use (globals.gCVars)
     return (HM.lookup name vars)

variableString :: B.ByteString -> Quake B.ByteString
variableString varName =
  do foundVar <- findVar varName
     return (maybe "" (^.cvString) foundVar)
