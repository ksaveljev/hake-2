{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Game.CVarT ( CVarT
                  , newCVarT
                  , cvName
                  , cvString
                  , cvLatchedString
                  , cvFlags
                  , cvModified
                  , cvValue
                  , cvNext
                  ) where

import Control.Lens (makeLenses)
import qualified Data.ByteString as B

data CVarT =
  CVarT { _cvName          :: B.ByteString
        , _cvString        :: B.ByteString
        , _cvLatchedString :: B.ByteString
        , _cvFlags         :: Int
        , _cvModified      :: Bool
        , _cvValue         :: Float
        , _cvNext          :: Maybe CVarT
        }

makeLenses ''CVarT

newCVarT :: CVarT
newCVarT = CVarT "" "" "" 0 False 0.0 Nothing
