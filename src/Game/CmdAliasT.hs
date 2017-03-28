{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Game.CmdAliasT where

import Control.Lens (makeLenses)
import qualified Data.ByteString as B

data CmdAliasT =
  CmdAliasT { _caName  :: B.ByteString
            , _caValue :: B.ByteString
            } deriving (Eq)

makeLenses ''CmdAliasT

newCmdAliasT :: CmdAliasT
newCmdAliasT =
  CmdAliasT { _caName  = ""
            , _caValue = ""
            }
