{-# LANGUAGE TemplateHaskell #-}
module QCommon.XCommandT ( XCommandT(..)
                         , module QCommon.XCommandT
                         ) where

import Control.Lens (makeLenses)

import Internal

makeLenses ''XCommandT
