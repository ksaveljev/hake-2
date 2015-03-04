{-# LANGUAGE TemplateHaskell #-}
module QCommon.TexInfoT where

import Linear.V4 (V4)
import Control.Lens (makeLenses)
import qualified Data.ByteString as B

data TexInfoT =
  TexInfoT { _texInfoSize        :: Int
           , _texInfoVecs        :: (V4 Float, V4 Float)
           , _texInfoFlags       :: Int
           , _texInfoValue       :: Int
           , _texInfoTexture     :: B.ByteString
           , _texInfoNextTexInfo :: Int
           }

makeLenses ''TexInfoT
