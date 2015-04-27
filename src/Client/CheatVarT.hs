{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Client.CheatVarT where

import Control.Lens (makeLenses)
import qualified Data.ByteString as B

data CheatVarT =
  CheatVarT { _chvName  :: B.ByteString
            , _chvValue :: B.ByteString
            }

makeLenses ''CheatVarT
