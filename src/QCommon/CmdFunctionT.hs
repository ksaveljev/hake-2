module QCommon.CmdFunctionT where

import qualified Data.ByteString as B

import QCommon.XCommandT

data CmdFunctionT =
  CmdFunctionT { cfNext     :: CmdFunctionT
               , cfName     :: B.ByteString
               , cfFunction :: XCommandT
               }
