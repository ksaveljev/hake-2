{-# LANGUAGE TemplateHaskell #-}
module Game.CmdGlobals
    ( module Game.CmdGlobals
    ) where

import           Control.Lens    (makeLenses)
import qualified Data.ByteString as B
import qualified Data.Sequence   as Seq
import qualified Data.Vector     as V

import qualified Constants
import           Types

makeLenses ''CmdGlobals

initialCmdGlobals :: CmdGlobals
initialCmdGlobals = CmdGlobals
    { _cgCmdFunctions = Seq.empty
    , _cgCmdArgc      = 0
    , _cgCmdArgv      = V.replicate Constants.maxStringTokens B.empty
    , _cgCmdArgs      = B.empty
    }