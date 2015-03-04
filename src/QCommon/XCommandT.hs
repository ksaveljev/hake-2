module QCommon.XCommandT where

import Control.Monad.State (StateT)

import QuakeState

type XCommandT = StateT QuakeState IO ()
