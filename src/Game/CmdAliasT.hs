{-# LANGUAGE TemplateHaskell #-}
module Game.CmdAliasT
  ( module Game.CmdAliasT
  ) where

import           Types

import           Control.Lens (makeLenses)
import qualified Data.ByteString as B

makeLenses ''CmdAliasT

newCmdAliasT :: CmdAliasT
newCmdAliasT =
  CmdAliasT { _caName  = B.empty
            , _caValue = B.empty
            }