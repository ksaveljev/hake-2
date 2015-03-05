{-# LANGUAGE TemplateHaskell #-}
module Game.CmdGlobals where

import Control.Lens (makeLenses)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import qualified Data.ByteString as B

import QCommon.XCommandT

data CmdGlobals =
  CmdGlobals { _cgCmdFunctions :: Seq XCommandT
             , _cgCmdArgc      :: Int
             , _cgCmdArgv      :: V.Vector B.ByteString
             }

makeLenses ''CmdGlobals

defaultCmdGlobals :: CmdGlobals
defaultCmdGlobals =
  CmdGlobals { _cgCmdFunctions = Seq.empty
             , _cgCmdArgc      = 0
             , _cgCmdArgv      = V.empty
             }
