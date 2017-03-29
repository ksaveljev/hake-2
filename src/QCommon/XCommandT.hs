{-# LANGUAGE TemplateHaskell #-}
module QCommon.XCommandT ( XCommandT(..)
                         , module QCommon.XCommandT
                         ) where

import Control.Lens (makeLenses)

import Types

makeLenses ''XCommandT
