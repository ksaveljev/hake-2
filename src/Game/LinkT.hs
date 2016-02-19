{-# LANGUAGE TemplateHaskell #-}
module Game.LinkT
  ( module Game.LinkT
  ) where

import Types

import Control.Lens (makeLenses)

makeLenses ''LinkT

newLinkT :: Int -> LinkT
newLinkT idx =
  LinkT { _lIndex = idx
        , _lPrev  = Nothing
        , _lNext  = Nothing
        , _lEdict = Nothing
        }