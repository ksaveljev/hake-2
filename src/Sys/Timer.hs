module Sys.Timer where

import Control.Lens
import Data.Functor ((<$>))
import Data.Time.Clock.POSIX

import Quake
import QuakeState

milliseconds :: Quake Int
milliseconds = do
    t <- io $ round <$> getPOSIXTime
    globals.curtime .= t
    return t
