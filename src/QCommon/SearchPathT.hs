{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module QCommon.SearchPathT ( module QCommon.SearchPathT
                           , module QCommon.PackT
                           ) where

import Control.Lens (makeLenses)
import qualified Data.ByteString as B

import QCommon.PackT

data SearchPathT =
  SearchPathT { _spFilename :: B.ByteString
              , _spPack     :: Maybe PackT
              } deriving (Eq)

makeLenses ''SearchPathT

newSearchPathT :: SearchPathT
newSearchPathT = SearchPathT "" Nothing
