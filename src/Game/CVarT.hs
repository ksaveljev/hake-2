{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Game.CVarT ( CVarT
                  , _cvName
                  , _cvString
                  , _cvLatchedString
                  , _cvFlags
                  , _cvModified
                  , _cvValue
                  , newCVarT
                  , cvName
                  , cvString
                  , cvLatchedString
                  , cvFlags
                  , cvModified
                  , cvValue
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
        }

makeLenses ''CVarT

newCVarT :: CVarT
newCVarT = CVarT "" "" "" 0 False 0.0
