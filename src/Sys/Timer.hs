module Sys.Timer where

import QuakeState
import Types

import Control.Lens ((.=))
import Data.Time.Clock.POSIX (getPOSIXTime)

milliseconds :: Quake Int
milliseconds =
  do t <- request (io (round . (* 1000) <$> getPOSIXTime))
     globals .= 1 -- TODO
     return t
