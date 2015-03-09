{-# LANGUAGE TemplateHaskell #-}
module QCommon.FileLinkT where

import Control.Lens (makeLenses)
import qualified Data.ByteString as B

data FileLinkT =
  FileLinkT { _flFrom       :: B.ByteString
            , _flFromLength :: Int
            , _flTo         :: B.ByteString
            }

makeLenses ''FileLinkT
