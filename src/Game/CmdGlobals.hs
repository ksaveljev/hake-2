{-# LANGUAGE TemplateHaskell #-}
module Game.CmdGlobals
  ( module Game.CmdGlobals
  ) where

import qualified Constants
import           Types

import           Control.Lens (makeLenses)
import qualified Data.Sequence as Seq
import qualified Data.Vector as V

makeLenses ''CmdGlobals

initialCmdGlobals :: CmdGlobals
initialCmdGlobals =
  CmdGlobals { _cgCmdFunctions = Seq.empty
             , _cgCmdArgc      = 0
             , _cgCmdArgv      = V.replicate Constants.maxStringTokens ""
             , _cgCmdArgs      = ""
             }