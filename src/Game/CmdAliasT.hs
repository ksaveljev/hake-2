module Game.CmdAliasT where

import qualified Data.ByteString as B

data CmdAliasT =
  CmdAliasT { caNext  :: Maybe CmdAliasT
            , caName  :: B.ByteString
            , caValue :: B.ByteString
            }
