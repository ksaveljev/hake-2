module QCommon.XCommandT where

import Control.Monad.State (StateT)
import Control.Monad.Except (ExceptT)
import qualified Data.ByteString as B

import QuakeState

type XCommandT = StateT QuakeState (ExceptT B.ByteString IO) ()
