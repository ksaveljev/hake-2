{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module QCommon.SearchPathT where

import Control.Lens (makeLenses)
import qualified Data.ByteString as B

import QCommon.PackT

data SearchPathT =
  SearchPathT { _spFilename :: B.ByteString
              , _spPack     :: PackT
              , _spNext     :: Maybe SearchPathT
              }

makeLenses ''SearchPathT

newSearchPathT :: SearchPathT
newSearchPathT = SearchPathT "" newPackT Nothing
