{-# LANGUAGE TemplateHaskell #-}
module Server.UCmdT where

import Control.Lens (makeLenses)
import qualified Data.ByteString as B

import QCommon.XCommandT

data UCmdT =
  UCmdT { _ucName :: B.ByteString
        , _ucFunc :: XCommandT
        }

makeLenses ''UCmdT
