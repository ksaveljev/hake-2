{-# LANGUAGE TemplateHaskell #-}
module Game.CmdAliasT where

import Control.Lens (makeLenses)
import qualified Data.ByteString as B

data CmdAliasT =
  CmdAliasT { _caNext  :: Maybe CmdAliasT
            , _caName  :: B.ByteString
            , _caValue :: B.ByteString
            }

makeLenses ''CmdAliasT
