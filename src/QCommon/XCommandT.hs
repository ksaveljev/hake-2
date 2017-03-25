{-# LANGUAGE TemplateHaskell #-}
module QCommon.XCommandT
    ( module QCommon.XCommandT
    ) where

import           Control.Lens (makeLenses)

import           Types

makeLenses ''XCommandT

runXCommandT :: XCommandT -> Quake ()
runXCommandT = _xcCmd