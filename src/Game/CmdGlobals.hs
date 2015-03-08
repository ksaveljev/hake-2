{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Game.CmdGlobals ( CmdGlobals
                       , initialCmdGlobals
                       , cgCmdFunctions
                       , cgCmdArgc
                       , cgCmdArgv
                       , cgCmdArgs
                       ) where

import Control.Lens (makeLenses)
import qualified Data.Sequence as Seq
import qualified Data.Vector as V

import Internal

makeLenses ''CmdGlobals

initialCmdGlobals :: CmdGlobals
initialCmdGlobals =
  CmdGlobals { _cgCmdFunctions = Seq.empty
             , _cgCmdArgc      = 0
             , _cgCmdArgv      = V.empty
             , _cgCmdArgs      = ""
             }
