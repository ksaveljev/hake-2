{-# LANGUAGE TemplateHaskell #-}
module Game.CmdGlobals ( CmdGlobals
                       , cgCmdFunctions
                       , cgCmdArgc
                       , cgCmdArgv
                       , defaultCmdGlobals
                       ) where

import Control.Lens (makeLenses)
import qualified Data.Sequence as Seq
import qualified Data.Vector as V

import Internal

makeLenses ''CmdGlobals

defaultCmdGlobals :: CmdGlobals
defaultCmdGlobals =
  CmdGlobals { _cgCmdFunctions = Seq.empty
             , _cgCmdArgc      = 0
             , _cgCmdArgv      = V.empty
             }
