{-# LANGUAGE TemplateHaskell #-}
module Game.CmdAliasT where

import           Control.Lens    (makeLenses)
import qualified Data.ByteString as B

import           Types

makeLenses ''CmdAliasT

newCmdAliasT :: CmdAliasT
newCmdAliasT = CmdAliasT
    { _caName  = ""
    , _caValue = ""
    }
