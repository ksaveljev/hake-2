{-# LANGUAGE TemplateHaskell #-}
module Game.CmdAliasT
  ( module Game.CmdAliasT
  ) where

import Types

import Control.Lens (makeLenses)

makeLenses ''CmdAliasT

newCmdAliasT :: CmdAliasT
newCmdAliasT =
  CmdAliasT { _caName  = ""
            , _caValue = ""
            }