module Game.CVarT where

import qualified Data.ByteString as B

data CVarT =
  CVarT { cvName          :: B.ByteString
        , cvString        :: B.ByteString
        , cvLatchedString :: B.ByteString
        , cvFlags         :: Int
        , cvModified      :: Bool
        , cvValue         :: Float
        , cvNext          :: Maybe CVarT
        }
