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
        , _cvLatchedString :: Maybe B.ByteString
        , _cvFlags         :: Int
        , _cvModified      :: Bool
        , _cvValue         :: Float
        } deriving (Eq)

makeLenses ''CVarT

newCVarT :: CVarT
newCVarT = CVarT "" "" Nothing 0 False 0.0
