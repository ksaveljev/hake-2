module Game.CmdAlias where

import qualified Data.ByteString as B

data CmdAlias = CmdAlias { cmdAliasNext  :: Maybe CmdAlias
                         , cmdAliasName  :: B.ByteString
                         , cmdAliasValue :: B.ByteString
                         }
