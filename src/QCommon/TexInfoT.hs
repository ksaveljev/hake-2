module QCommon.TexInfoT where

import Linear.V4 (V4)
import qualified Data.ByteString as B

data TexInfoT =
  TexInfoT { texInfoSize        :: Int
           , texInfoVecs        :: (V4 Float, V4 Float)
           , texInfoFlags       :: Int
           , texInfoValue       :: Int
           , texInfoTexture     :: B.ByteString
           , texInfoNextTexInfo :: Int
           }
